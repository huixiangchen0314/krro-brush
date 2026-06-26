(ns top.kzre.krro.brush.dynamics
  "动力学映射器：将传感器值（压力、速度、倾斜等）转换为 dab 参数。
   所有函数均为纯数据变换，不依赖外部状态。"
  (:require [top.kzre.krro.brush.util :as util]))

;; ── 曲线函数库 ───────────────────────────────────────
(defn apply-curve
  "根据曲线类型，将原始值 x (0-1) 映射到另一个 0-1 值。"
  [x curve-type]
  (case curve-type
    :linear    (double x)
    :sigmoid   (util/sigmoid x)
    :quadratic (* x x)
    :cubic     (* x x x)
    :sqrt      (Math/sqrt x)
    :inv-sigmoid (let [s (util/sigmoid (- 1.0 x))] s)  ;; 反转 S 型
    ;; 默认线性
    (double x)))

;; ── 主映射函数 ────────────────────────────────────────
(defn map-dynamics
  "根据动力学规格 dynamics-spec 和当前传感器事件 input-event，
   对 dab 基础参数 dab-base 进行动力学放大，返回更新后的 dab 参数 map。
   dynamics-spec 结构示例：
     {:pressure {:radius {:curve :linear, :min 0.5, :max 1.0}
                 :opacity {:curve :sigmoid, :min 0.2, :max 1.0}}
      :velocity {:spacing {:curve :linear, :min 0.5, :max 1.5}}}
   input-event 应至少包含 :pressure, :velocity 等字段（缺失时使用默认值 1.0）。"
  [dab-base dynamics-spec input-event]
  (reduce-kv
    (fn [params sensor-name mapping]
      ;; 从输入事件中获取传感器原始值
      (let [raw (case sensor-name
                  :pressure (get input-event :pressure 1.0)
                  :velocity (get input-event :velocity 1.0)
                  :tilt-x   (get-in input-event [:tilt :x] 0.5)
                  :tilt-y   (get-in input-event [:tilt :y] 0.5)
                  :rotation (get input-event :rotation 0.0)
                  ;; 默认值 1.0 表示无影响
                  1.0)]
        ;; 对每个目标参数（如 :radius, :opacity）进行映射
        (reduce-kv
          (fn [params param-name {:keys [curve min max] :or {curve :linear, min 0.0, max 1.0}}]
            (let [mapped (+ min (* (- max min) (apply-curve raw curve)))]
              (assoc params param-name mapped)))
          params
          mapping)))
    dab-base
    dynamics-spec))