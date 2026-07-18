(ns top.kzre.krro.brush.resample
  "事件重采样与插值，保证 dab 等距分布并平滑其他动态属性。
   假设所有事件已通过 make-event 补全默认字段。"
  (:require [top.kzre.krro.brush.util :as util]))

(defn spacing-resample
  "原始下采样：按间距跳过过密的事件点。
   spacing 为间距系数（乘以 dab 直径），radius 为笔刷半径。"
  [events spacing radius]
  (let [step (* 2 radius spacing)]
    (loop [remaining events
           result   []
           last-x nil
           last-y nil]
      (if (empty? remaining)
        result
        (let [ev  (first remaining)
              x   (:x ev)
              y   (:y ev)]
          (if (or (nil? last-x)
                  (>= (util/distance [last-x last-y] [x y]) step))
            (recur (rest remaining) (conj result ev) x y)
            (recur (rest remaining) result last-x last-y)))))))

(defn- interpolate-between
  "在 p1 和 p2 之间每隔 step 像素生成一个插值点（包含 p2，但不包含 p1）。
   直接对已知连续字段进行线性插值：:x :y :pressure :velocity :timestamp
   :rotation :tilt-x :tilt-y."
  [p1 p2 step]
  (let [dist (util/distance [(:x p1) (:y p1)] [(:x p2) (:y p2)])]
    (if (< dist 1e-6)
      []
      (let [n (max 1 (int (Math/floor (/ dist step))))
            x1 (:x p1) x2 (:x p2)
            y1 (:y p1) y2 (:y p2)
            pr1 (:pressure p1) pr2 (:pressure p2)
            v1  (:velocity p1) v2  (:velocity p2)
            ts1 (:timestamp p1) ts2 (:timestamp p2)
            rot1 (:rotation p1) rot2 (:rotation p2)
            tx1 (:tilt-x p1) tx2 (:tilt-x p2)
            ty1 (:tilt-y p1) ty2 (:tilt-y p2)]
        (map (fn [i]
               (let [t (/ i n)]
                 {:x         (util/lerp x1 x2 t)
                  :y         (util/lerp y1 y2 t)
                  :pressure  (util/lerp pr1 pr2 t)
                  :velocity  (util/lerp v1 v2 t)
                  :timestamp (long (util/lerp (double ts1) (double ts2) t))
                  :rotation  (util/lerp rot1 rot2 t)
                  :tilt-x    (util/lerp tx1 tx2 t)
                  :tilt-y    (util/lerp ty1 ty2 t)}))
             (range 1 (inc n)))))))

(defn resample
  "通用重采样 + 插值。
   输入 events（由 make-event 构造的完整事件），spacing 系数，radius 半径。
   返回新的均匀事件序列，相邻点距离约为 step = 2 * radius * spacing。
   原始点全部保留，距离过长的段会插入线性插值点。"
  [events brush-spec]
  (if (<= (count events) 1)
    events
    (let [step (* 2 (:radius brush-spec 5) (:spacing brush-spec 0.5))]
      (loop [remaining (rest events)
             result    [(first events)]
             prev      (first events)]
        (if (empty? remaining)
          result
          (let [curr (first remaining)
                inter-points (interpolate-between prev curr step)]
            (recur (rest remaining)
                   (into result inter-points)
                   curr)))))))