(ns top.kzre.krro.brush.mix
  "内置基础混色器（测试用），实现 IColorMixer 协议。
   仅提供简单的前景色覆盖，真正的颜色混合应由宿主注入。"
  (:require [top.kzre.krro.brush.protocol :as p]
            [top.kzre.krro.brush.util :as util]))

(defrecord DefaultMixer []
  p/IColorMixer
  (mix-colors [_ fg bg opacity blend-mode]
    (let [fg-rgb (pop fg) bg-rgb (pop bg)
          fg-a   (peek fg) bg-a   (peek bg)
          ;; 将笔刷不透明度应用到前景 alpha
          eff-fg-a (* fg-a opacity)
          a-out  (+ eff-fg-a (* bg-a (- 1 eff-fg-a)))
          inv-out (if (zero? a-out) 0.0 (/ 1.0 a-out))]
      (if (zero? a-out)
        [0.0 0.0 0.0 0.0]
        [(/ (+ (* (first fg-rgb) eff-fg-a) (* (first bg-rgb) bg-a (- 1 eff-fg-a))) a-out)
         (/ (+ (* (second fg-rgb) eff-fg-a) (* (second bg-rgb) bg-a (- 1 eff-fg-a))) a-out)
         (/ (+ (* (nth fg-rgb 2) eff-fg-a) (* (nth bg-rgb 2) bg-a (- 1 eff-fg-a))) a-out)
         a-out])))
  (mix-pigments [_ pigment-keys ratios]
    [0.0 0.0 0.0]))

(def default-mixer (->DefaultMixer))