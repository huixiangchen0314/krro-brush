(ns top.kzre.krro.brush.smoother
  "平滑器实现：高斯平滑、间距重采样、自适应 Kalman 滤波器、Cable Stabilizer (EMA 拉线)。
   输入事件为普通 map，至少包含 :x :y :timestamp。"
  (:require [top.kzre.krro.brush.util :as util]))

;; ── 工具函数 ─────────────────────────────────────────
(defn- velocity-between [p1 p2]
  (let [dx (- (:x p2) (:x p1))
        dy (- (:y p2) (:y p1))
        dt (- (:timestamp p2) (:timestamp p1))
        dt (max dt 1.0) ;; 防止除零
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (/ dist dt)))

;; ── 高斯平滑 ─────────────────────────────────────────
(defn- gaussian-smooth
  [pts window-size]
  (let [half (quot window-size 2)
        n (count pts)]
    (map-indexed
      (fn [i pt]
        (let [start (max 0 (- i half))
              end   (min n (+ i half 1))
              window (subvec (vec pts) start end)
              avg-pos (util/avg-point (map :position pts))]
          (assoc pt :position avg-pos)))
      pts)))

;; ── 间距重采样 ─────────────────────────────────────
(defn- spacing-resample
  [pts spacing diameter]
  (let [step (* spacing diameter)]
    (loop [remaining pts
           result   []
           last-pos nil]
      (if (empty? remaining)
        result
        (let [pt  (first remaining)
              pos (:position pt)]
          (if (or (nil? last-pos) (>= (util/distance last-pos pos) step))
            (recur (rest remaining) (conj result pt) pos)
            (recur (rest remaining) result last-pos)))))))

;; ── Kalman 平滑 (支持动态噪声) ────────────────────────
(defn- kalman-smooth-points
  "对点序列进行 Kalman 滤波。:velocity-adaptive 为真时，根据速度动态调整过程噪声。"
  [pts process-noise measurement-noise & {:keys [velocity-adaptive?]
                                          :or {velocity-adaptive? false}}]
  (let [;; 初始状态
        init-x {:estimate (double (:x (first pts))) :error 1.0}
        init-y {:estimate (double (:y (first pts))) :error 1.0}
        velocities (mapv #(velocity-between (nth pts %1) (nth pts %2)) (range (dec (count pts))) (range 1 (count pts)))
        avg-vel (if (pos? (count velocities))
                  (/ (apply + velocities) (count velocities))
                  0.1)
        smooth-1d (fn [init-state values]
                    (rest
                      (reduce
                        (fn [acc val]
                          (let [{:keys [estimate error]} (peek acc)
                                ;; 预测
                                predicted estimate
                                ;; 动态噪声：速度越高，过程噪声越大（平滑减弱）
                                actual-process-noise
                                (if velocity-adaptive?
                                  (let [vel (nth (cons avg-vel velocities) (dec (count acc)) avg-vel)]
                                    (+ process-noise (* process-noise (* vel 10.0))))
                                  process-noise)
                                predicted-error (+ error actual-process-noise)
                                ;; 更新
                                gain (/ predicted-error (+ predicted-error measurement-noise))
                                new-estimate (+ predicted (* gain (- val predicted)))
                                new-error (* (- 1 gain) predicted-error)]
                            (conj acc {:estimate new-estimate :error new-error})))
                        [{:estimate (first values) :error 1.0}]
                        values)))
        xs (map :x pts)
        ys (map :y pts)
        smooth-xs (smooth-1d init-x xs)
        smooth-ys (smooth-1d init-y ys)]
    (mapv (fn [pt sx sy] (assoc pt :x (:estimate sx) :y (:estimate sy)))
          pts smooth-xs smooth-ys)))

;; ── Cable Stabilizer (EMA 拉线平滑) ──────────────────
(defn- cable-smooth
  "指数移动平均拉线平滑。strength 0-1，值越小越平滑但延迟越大。"
  [pts strength]
  (let [alpha (max 0.01 (min 1.0 strength))]
    (rest
      (reduce
        (fn [acc pt]
          (let [prev (peek acc)
                x (:x pt) y (:y pt)
                new-x (+ (:x prev) (* alpha (- x (:x prev))))
                new-y (+ (:y prev) (* alpha (- y (:y prev))))]
            (conj acc (assoc pt :x new-x :y new-y))))
        [(first pts)] ;; 初始点
        pts))))

;; ── 锥化 (Taper) ──────────────────────────────────────
(defn- apply-taper
  "给 dab 序列添加 :taper 字段，控制两端淡入淡出。
   taper-start 和 taper-end 为 0-1，表示锥化长度比例。"
  [dabs taper-start taper-end]
  (let [n (count dabs)
        start-len (int (* n taper-start))
        end-len (int (* n taper-end))]
    (map-indexed
      (fn [i dab]
        (let [t (cond
                  (< i start-len) (/ (double i) (max 1 start-len))
                  (>= i (- n end-len)) (/ (double (- n 1 i)) (max 1 end-len))
                  :else 1.0)]
          (assoc dab :taper t)))
      dabs)))

;; ── 主平滑函数 ──────────────────────────────────────
(defn smooth
  "实现 ISmoother 协议。
   stroke-spec 可包含：
     :stabilizer     - :gaussian, :kalman, :cable
     :smoothing      - 窗口大小（高斯）或 process-noise（Kalman）或 cable-strength
     :measurement-noise - Kalman 测量噪声
     :velocity-adaptive? - 是否使用速度自适应 Kalman
     :spacing        - dab 间距（占直径比例）
     :dab            - 含 :radius
     :taper-start, :taper-end - 锥化比例 (0-1)，默认 0.0"
  [input-events stroke-spec]
  (let [stabilizer   (get stroke-spec :stabilizer :gaussian)
        spacing      (get stroke-spec :spacing 0.1)
        diameter     (* 2.0 (get-in stroke-spec [:dab :radius] 10.0))
        taper-start  (get stroke-spec :taper-start 0.0)
        taper-end    (get stroke-spec :taper-end 0.0)
        pts (vec input-events)
        ;; 选择平滑算法
        smoothed (case stabilizer
                   :kalman (kalman-smooth-points pts
                                                 (get stroke-spec :smoothing 0.01)
                                                 (get stroke-spec :measurement-noise 0.1)
                                                 :velocity-adaptive? (get stroke-spec :velocity-adaptive? false))
                   :cable  (cable-smooth pts (get stroke-spec :smoothing 0.3))
                   ;; 默认高斯
                   (gaussian-smooth pts (int (get stroke-spec :smoothing 3))))
        ;; 间距重采样
        resampled (spacing-resample smoothed spacing diameter)
        ;; 锥化
        final (if (or (pos? taper-start) (pos? taper-end))
                (apply-taper resampled taper-start taper-end)
                resampled)]
    final))