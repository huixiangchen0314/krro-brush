(ns top.kzre.krro.brush.rdp
  "Ramer–Douglas–Peucker 事件降采样算法。
   用于减少笔刷事件点的数量，在保持形状的前提下提升渲染性能。
   包含最小距离预过滤、首尾点保护和安全的保留长度处理。")

(defn- filter-close-points [points min-dist]
  (reduce (fn [acc pt]
            (if-let [last (peek acc)]
              (let [dx (- (:x pt) (:x last))
                    dy (- (:y pt) (:y last))
                    d  (Math/sqrt (+ (* dx dx) (* dy dy)))]
                (if (> d min-dist) (conj acc pt) acc))
              (conj acc pt)))
          []
          points))

(defn- perpendicular-distance
  "计算点 [x y] 到线段 (x1,y1)-(x2,y2) 的垂直距离。"
  [x y x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        mag (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (if (zero? mag)
      (Math/sqrt (+ (Math/pow (- x x1) 2) (Math/pow (- y y1) 2)))
      (Math/abs (double (/ (+ (* dy x) (- (* dx y)) (- (* dy x1)) (* dx y1)) mag))))))

(defn- rdp
  "RDP 递归简化。points 为向量 [{:x :y ...}]，保留首尾点。
   增加 max-points 参数：当点数少于该值时直接返回原序列，避免过度简化。"
  [points epsilon & {:keys [max-points] :or {max-points 3}}]
  (if (< (count points) max-points)
    points
    (let [first-pt (first points)
          last-pt  (last points)]
      (loop [i 1, dist-max 0.0, idx-max 0]
        (if (< i (dec (count points)))
          (let [pt (nth points i)
                d  (perpendicular-distance (:x pt) (:y pt)
                                           (:x first-pt) (:y first-pt)
                                           (:x last-pt) (:y last-pt))]
            (if (> d dist-max)
              (recur (inc i) d i)
              (recur (inc i) dist-max idx-max)))
          (if (> dist-max epsilon)
            (let [left  (rdp (subvec points 0 (inc idx-max)) epsilon :max-points max-points)
                  right (rdp (subvec points idx-max) epsilon :max-points max-points)]
              (into (pop left) right))
            [first-pt last-pt]))))))

(defn- rdp-segment
  "对中间部分应用 RDP，同时确保首尾保留点不重叠。
   preserve-head, preserve-tail 会被限制在合理范围内。"
  [points epsilon preserve-head preserve-tail]
  (let [total (count points)]
    (if (<= total 0)
      points
      (let [head-len (min preserve-head total)
            tail-len (min preserve-tail (max 0 (- total head-len)))
            head     (take head-len points)
            tail     (take-last tail-len points)
            middle   (if (and (< head-len total) (< (+ head-len tail-len) total))
                       (drop head-len (drop-last tail-len points))
                       [])]
        (if (empty? middle)
          (vec points)   ;; 无法分割，保留全部
          (into (vec head) (into (rdp (vec middle) epsilon) tail)))))))

(defn simplify
  "对事件序列进行简化：先通过最小距离过滤，再对中间部分应用 RDP 降采样，
   强制保留首尾 preserve-head 和 preserve-tail 个点。
   events       : 事件序列 [{:x :y ...}]
   epsilon      : RDP 距离阈值（像素），值越小保留的点越多
   min-dist     : 最小点间距（像素），小于此距离的点将被合并（可选，默认为 nil）
   preserve-head: 头部强制保留的点数（可选，默认 3）
   preserve-tail: 尾部强制保留的点数（可选，默认 3）"
  [events epsilon & {:keys [min-dist preserve-head preserve-tail]
                     :or {preserve-head 3 preserve-tail 3}}]
  (let [filtered (if min-dist (filter-close-points events min-dist) events)]
    (if (or (empty? filtered) (not (pos? epsilon)))
      filtered
      (rdp-segment (vec filtered) epsilon preserve-head preserve-tail))))