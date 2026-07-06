(ns top.kzre.krro.brush.event
  "事件工具函数：构造并立即补全默认值。")

(defn polyfill
  "补全事件缺失的默认字段，保证所有连续属性都有合理的值。
   默认值：pressure 0.5, velocity 0.5, timestamp 当前毫秒,
   rotation 0.0, tilt-x 0.0, tilt-y 0.0。"
  [event]
  (let [defaults {:pressure 0.5
                  :velocity 0.5
                  :timestamp (System/currentTimeMillis)
                  :rotation 0.0
                  :tilt-x 0.0
                  :tilt-y 0.0}]
    (merge defaults event)))

(defn make-event
  "构造一个画笔事件 map 并立即补全默认值。
   必填 :x :y，可选 :pressure :velocity :timestamp :rotation :tilt-x :tilt-y 等。"
  [x y & {:keys [pressure velocity timestamp rotation tilt-x tilt-y] :as opts}]
  (polyfill (merge {:x x :y y} opts)))