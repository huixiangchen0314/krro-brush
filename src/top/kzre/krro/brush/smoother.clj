(ns top.kzre.krro.brush.smoother
  "平滑器实现：高斯平滑 + 间距重采样 + Kalman 滤波器。
   输入事件为普通 map，至少包含 :x :y :timestamp。"
  (:require [top.kzre.krro.brush.util :as util]))

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

;; ── Kalman 滤波器（简化 1D） ─────────────────────────
(defn- kalman-smooth-points
  "对点序列的 x 和 y 坐标分别应用一维 Kalman 滤波器，返回平滑后的点序列。
   process-noise 和 measurement-noise 控制平滑程度，值越小越平滑但延迟越大。"
  [pts process-noise measurement-noise]
  (let [;; 初始状态
        init-x {:estimate (double (:x (first pts))) :error 1.0}
        init-y {:estimate (double (:y (first pts))) :error 1.0}
        smooth-1d (fn [init-state values]
                    (rest
                      (reduce
                        (fn [acc val]
                          (let [{:keys [estimate error]} (peek acc)
                                ;; 预测
                                predicted estimate
                                predicted-error (+ error process-noise)
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

;; ── 主平滑函数 ──────────────────────────────────────
(defn smooth
  "实现 ISmoother 协议。
   stroke-spec 可包含：
     :stabilizer - :gaussian 或 :kalman
     :smoothing  - 窗口大小（高斯）或 process-noise（Kalman）
     :measurement-noise - Kalman 测量噪声（默认 0.1）
     :spacing    - dab 间距（占直径比例）
     :dab        - 含 :radius"
  [input-events stroke-spec]
  (let [stabilizer (get stroke-spec :stabilizer :gaussian)
        spacing    (get stroke-spec :spacing 0.1)
        diameter   (* 2.0 (get-in stroke-spec [:dab :radius] 10.0))
        pts (vec input-events)]
    (case stabilizer
      :kalman (let [proc-noise (get stroke-spec :smoothing 0.01)
                    meas-noise (get stroke-spec :measurement-noise 0.1)]
                (-> (kalman-smooth-points pts proc-noise meas-noise)
                    (spacing-resample spacing diameter)))
      ;; 默认高斯
      (let [window (int (get stroke-spec :smoothing 3))]
        (-> (gaussian-smooth pts window)
            (spacing-resample spacing diameter))))))