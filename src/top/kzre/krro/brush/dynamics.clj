(ns top.kzre.krro.brush.dynamics
  "动力学映射器：将传感器值（压力、速度、倾斜等）转换为 dab 参数。
   支持直接设置 (:set) 与乘法叠加 (:multiply) 两种模式。
   所有函数均为纯数据变换，不依赖外部状态。"
  (:import (top.kzre.curve.bezier2d Curve)
           (top.kzre.valuemapping Mappers ValueMapper)
           (top.kzre.colorutils MathUtils))
  (:require
    [top.kzre.krro.brush.util :as util]))

;; ── 传感器值提取多方法 ──────────────────────────────
(defmulti sensor-value
          (fn [event-param _event] event-param))

(defmethod sensor-value :pressure
  [_ event]
  (let [v (:pressure event 1.0)]
    (if (number? v) (double v) 1.0)))

(defmethod sensor-value :velocity
  [_ event]
  (let [v (:velocity event 1.0)]
    (if (number? v) (double v) 1.0)))

(defmethod sensor-value :tilt-x
  [_ event]
  (let [v (get-in event [:tilt :x] 0.0)
        max-tilt 60.0
        normalized (/ (double v) max-tilt)]
    (MathUtils/clamp01 (+ 0.5 (/ normalized 2.0)))))

(defmethod sensor-value :tilt-y
  [_ event]
  (let [v (get-in event [:tilt :y] 0.0)
        max-tilt 60.0
        normalized (/ (double v) max-tilt)]
    (MathUtils/clamp01 (+ 0.5 (/ normalized 2.0)))))

(defmethod sensor-value :rotation
  [_ event]
  (let [v (:rotation event 0.0)]
    (if (number? v)
      (/ (mod (double v) 360.0) 360.0)
      0.0)))

(defmethod sensor-value :default [_ _] 1.0)

;; ── 曲线映射 ──────────────────────────────────────
(def ^:private curve-mappers
  {:linear       (Mappers/linear)
   :sigmoid      (Mappers/sigmoid)
   :quadratic    (Mappers/quadratic)
   :cubic        (Mappers/cubic)
   :sqrt         (Mappers/sqrt)
   :inv-sigmoid  (Mappers/inverseSigmoid)})

(defn- get-mapper [curve-type opts]
  (case curve-type
    (:linear :sigmoid :quadratic :cubic :sqrt :inv-sigmoid)
    (curve-mappers curve-type)

    :bezier
    (if-let [^Curve curve (:bezier-curve opts)]
      (Mappers/bezier curve)
      (throw (ex-info "Bezier curve is required" {:opts opts})))

    :lookup
    (if-let [table (:table opts)]
      (Mappers/lookupTable (double-array (mapv first table))
                           (double-array (mapv second table)))
      (throw (ex-info "Lookup table is required" {:opts opts})))

    (curve-mappers :linear)))

(defn apply-curve [value curve-type opts]
  (let [^ValueMapper mapper (get-mapper curve-type opts)]
    (.map mapper (double value))))

;; ── 主映射函数 ──────────────────────────────────────
(defn map-dynamics
  "根据动力学规格 brush-spec 和当前事件 event，对事件参数进行映射。
   支持两种配置格式（目标参数优先）：
     1. 单个传感器映射：
        {:radius {:sensor :pressure, :curve :linear, :min 0.2, :max 1.0, :mode :multiply}}
     2. 多个传感器映射同一参数：
        {:radius [{:sensor :pressure, :curve :linear, :min 0.2, :max 1.0, :mode :multiply}
                  {:sensor :velocity, :curve :sqrt, :min 0.5, :max 1.0}]}
   :mode 可选值：
     :set      （默认）直接将映射结果设置为参数值。
     :multiply 将映射结果作为乘子，乘以事件原有值后更新。若事件中不存在该参数则抛出异常。"
  [event brush-spec]
  (reduce-kv
    (fn [acc-params param-kw config]
      (let [configs (if (map? config) [config] config)]  ;; 统一为向量
        (reduce (fn [params {:keys [sensor curve min max mode] :as opts
                             :or {curve :linear, min 0.0, max 1.0, mode :set}}]
                  (let [raw-value (sensor-value sensor event)
                        curved    (apply-curve raw-value curve (dissoc opts :sensor :curve :min :max :mode))
                        mapped    (+ (double min) (* (- (double max) (double min)) curved))]
                    (case mode
                      :set
                      (assoc params param-kw mapped)

                      :multiply
                      (if-let [current (util/param param-kw params brush-spec)]
                        (assoc params param-kw (* current mapped))
                        (throw (ex-info (str "Cannot multiply: parameter " param-kw " not present in event.")
                                        {:event params :missing-param param-kw})))

                      (throw (ex-info (str "Unknown dynamics mode: " mode) {:mode mode})))))
                acc-params
                configs)))
    event
    (:dynamics brush-spec)))