(ns top.kzre.krro.brush.dynamics
  "动力学映射器：将传感器值（压力、速度、倾斜等）转换为 dab 参数。
   所有函数均为纯数据变换，不依赖外部状态。"
  (:require [top.kzre.krro.brush.util :as util]))

;; ── 传感器值提取多方法 ──────────────────────────────
(defmulti sensor-value
          "根据传感器名称从输入事件中提取原始值。
           sensor-name : 传感器关键字，如 :pressure, :velocity, :tilt-x, :tilt-y, :rotation
           opts        : map，必须包含 :input-event，可选包含 :default"
          (fn [sensor-name opts] sensor-name))

(defmethod sensor-value :pressure
  [_ {:keys [input-event]}]
  (let [v (:pressure input-event 1.0)]
    (if (number? v) (double v) 1.0)))

(defmethod sensor-value :velocity
  [_ {:keys [input-event]}]
  (let [v (:velocity input-event 1.0)]
    (if (number? v) (double v) 1.0)))

;; 倾斜 X/Y 归一化：通常倾斜角度在 -60° 到 60° 之间，
;; 我们将其映射到 0~1 范围（0.5 表示垂直，0 表示向左/上最大倾斜，1 表示向右/下最大倾斜）
(defmethod sensor-value :tilt-x
  [_ {:keys [input-event]}]
  (let [v (get-in input-event [:tilt :x] 0.0)
        max-tilt 60.0
        normalized (/ (double v) max-tilt)]
    (util/clamp 0.0 1.0 (+ 0.5 (/ normalized 2.0)))))

(defmethod sensor-value :tilt-y
  [_ {:keys [input-event]}]
  (let [v (get-in input-event [:tilt :y] 0.0)
        max-tilt 60.0
        normalized (/ (double v) max-tilt)]
    (util/clamp 0.0 1.0 (+ 0.5 (/ normalized 2.0)))))

;; 旋转：0~360 度，映射到 0~1
(defmethod sensor-value :rotation
  [_ {:keys [input-event]}]
  (let [v (:rotation input-event 0.0)]
    (if (number? v)
      (/ (mod (double v) 360.0) 360.0)
      0.0)))

(defmethod sensor-value :default
  [_ _]
  1.0)

;; ── 曲线映射多方法 ──────────────────────────────────
(defmulti apply-curve
          "根据 curve-type 将原始值 x (0-1) 映射到另一个 0-1 值。
           x          : 待映射的数值
           curve-type : 曲线关键字，如 :linear, :sigmoid, :bezier, :lookup 等
           opts       : 可选配置 map，不同曲线需要不同参数"
          (fn [x curve-type opts] curve-type))

(defmethod apply-curve :linear [x _ _] (double x))
(defmethod apply-curve :sigmoid [x _ _] (util/sigmoid x))
(defmethod apply-curve :quadratic [x _ _] (* x x))
(defmethod apply-curve :cubic [x _ _] (* x x x))
(defmethod apply-curve :sqrt [x _ _] (Math/sqrt x))
(defmethod apply-curve :inv-sigmoid [x _ _] (- 1.0 (util/sigmoid x)))

(defmethod apply-curve :bezier
  [x _ {:keys [c1 c2] :or {c1 0.25 c2 0.75}}]
  (let [t (util/clamp 0.0 1.0 x)
        u (- 1 t)
        y (+ (* 3 u u t c1)
             (* 3 u t t c2)
             (* t t t))]
    (util/clamp 0.0 1.0 y)))

(defmethod apply-curve :lookup
  [x _ {:keys [table] :or {table [0.0 1.0]}}]
  (let [n (count table)
        idx (* x (dec n))
        i (int idx)
        t (- idx i)
        v1 (nth table i)
        v2 (nth table (min (inc i) (dec n)))]
    (util/lerp v1 v2 t)))

(defmethod apply-curve :default [x _ _] (double x))

;; ── 主映射函数 ──────────────────────────────────────
(defn map-dynamics
  "根据动力学规格 dynamics-spec 和当前传感器事件 input-event，
   对 dab 基础参数 dab-base 进行动力学放大，返回更新后的 dab 参数 map。
   注意：映射后的值**不会**被自动钳制到 [0,1]，由调用方根据需要处理。"
  [dab-base dynamics-spec input-event]
  (reduce-kv
    (fn [params sensor-name mapping]
      (let [raw (sensor-value sensor-name {:input-event input-event})]
        (reduce-kv
          (fn [params param-name {:keys [curve min max] :as opts :or {curve :linear, min 0.0, max 1.0}}]
            (let [curved (apply-curve raw curve (dissoc opts :curve :min :max))
                  mapped (+ (double min) (* (- (double max) (double min)) curved))]
              (assoc params param-name mapped)))
          params
          mapping)))
    dab-base
    dynamics-spec))