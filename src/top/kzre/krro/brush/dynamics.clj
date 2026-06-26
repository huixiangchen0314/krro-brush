(ns top.kzre.krro.brush.dynamics
  "动力学映射器：将传感器值（压力、速度、倾斜等）转换为 dab 参数。
   所有函数均为纯数据变换，不依赖外部状态。"
  (:require [top.kzre.krro.brush.util :as util]))

(defmulti apply-curve
          "根据 curve-type 将原始值 x (0-1) 映射到另一个 0-1 值。"
          (fn [x curve-type] curve-type))

(defmethod apply-curve :linear [x _] (double x))
(defmethod apply-curve :sigmoid [x _] (util/sigmoid x))
(defmethod apply-curve :quadratic [x _] (* x x))
(defmethod apply-curve :cubic [x _] (* x x x))
(defmethod apply-curve :sqrt [x _] (Math/sqrt x))
(defmethod apply-curve :inv-sigmoid [x _] (util/sigmoid (- 1.0 x)))
(defmethod apply-curve :default [x _] (double x))

;; 新增 bezier 和 lookup 曲线方法
(defmethod apply-curve :bezier
  [x {:keys [c1 c2] :or {c1 0.25 c2 0.75}}]
  ;; 使用三次贝塞尔曲线（由两个控制点 c1,c2 定义，范围 0-1）
  (let [t (util/clamp 0.0 1.0 x)
        ;; 简化的贝塞尔计算：B(t) = 3*(1-t)^2*t*c1 + 3*(1-t)*t^2*c2 + t^3
        u (- 1 t)
        y (+ (* 3 u u t c1)
             (* 3 u t t c2)
             (* t t t))]
    (util/clamp 0.0 1.0 y)))

(defmethod apply-curve :lookup
  [x table]
  (let [n (count table)
        idx (* x (dec n))
        i (int idx)
        t (- idx i)
        v1 (nth table i)
        v2 (nth table (min (inc i) (dec n)))]
    (util/lerp v1 v2 t)))

(defn map-dynamics
  "根据动力学规格 dynamics-spec 和当前传感器事件 input-event，
   对 dab 基础参数 dab-base 进行动力学放大，返回更新后的 dab 参数 map。"
  [dab-base dynamics-spec input-event]
  (reduce-kv
    (fn [params sensor-name mapping]
      (let [raw (case sensor-name
                  :pressure (get input-event :pressure 1.0)
                  :velocity (get input-event :velocity 1.0)
                  :tilt-x   (get-in input-event [:tilt :x] 0.5)
                  :tilt-y   (get-in input-event [:tilt :y] 0.5)
                  :rotation (get input-event :rotation 0.0)
                  1.0)]
        (reduce-kv
          (fn [params param-name {:keys [curve min max] :or {curve :linear, min 0.0, max 1.0}}]
            (let [mapped (+ min (* (- max min) (apply-curve raw curve)))]
              (assoc params param-name mapped)))
          params
          mapping)))
    dab-base
    dynamics-spec))