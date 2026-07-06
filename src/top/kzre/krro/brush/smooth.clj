(ns top.kzre.krro.brush.smooth
  "平滑器实现：基于 top.kzre.smooth 库的在线二维滤波器。
   仅负责对输入事件序列的 :x / :y 进行低通滤波，其他字段原样保留。
   不进行重采样、渐细等操作。"
  (:import (top.kzre.smooth Filter2D)
           (top.kzre.smooth GaussianWindowFilter2D)
           (top.kzre.smooth KalmanFilter2D)
           (top.kzre.smooth CableFilter2D)))

;; ── 辅助：在线滤波批量事件 ───────────────────────
(defn- filter-events [^Filter2D filter2d state-factory events]
  (let [state (state-factory)]
    (mapv (fn [e]
            (let [x  (double (:x e))
                  y  (double (:y e))
                  _  (.filter filter2d x y state)]
              (assoc e
                :x (.getFilteredX state)
                :y (.getFilteredY state))))
          events)))

;; ── 多方法分派 ───────────────────────────────────
(defmulti smooth
          "根据 smooth-spec 中的 :stabilizer 选择平滑算法，返回平滑后的事件序列。"
          (fn [_input-events smooth-spec] (get smooth-spec :stabilizer :gaussian)))

(defmethod smooth :gaussian [input-events smooth-spec]
  (let [window       (int (get smooth-spec :smoothing 5))
        sigma        (get smooth-spec :sigma 1.5)
        ^Filter2D f2d (GaussianWindowFilter2D/newInstance window sigma)
        state-factory #(GaussianWindowFilter2D/newState window)]
    (filter-events f2d state-factory input-events)))

(defmethod smooth :kalman [input-events smooth-spec]
  (let [first-event   (first input-events)
        proc-noise   (get smooth-spec :process-noise 0.01)
        meas-noise   (get smooth-spec :measurement-noise 0.1)
        init-x       (:x first-event)
        init-y       (:y first-event)
        init-vx      0.0
        init-vy      0.0
        init-error   1.0
        ^Filter2D f2d (KalmanFilter2D/instance)
        state-factory #(KalmanFilter2D/newState proc-noise meas-noise init-x init-y init-vx init-vy init-error)]
    (filter-events f2d state-factory input-events)))

(defmethod smooth :cable [input-events smooth-spec]
  (let [alpha        (get smooth-spec :smoothing 0.3)
        ^Filter2D f2d (CableFilter2D/instance)
        state-factory #(CableFilter2D/newState alpha)]
    (filter-events f2d state-factory input-events)))

;; 默认不平滑
(defmethod smooth :default [input-events _smooth-spec]
  input-events)