(ns top.kzre.krro.brush.mix
  "内置基础混色器（测试用），实现 IColorMixer 协议。
   仅提供简单的前景色覆盖，真正的颜色混合应由宿主注入。"
  (:require [top.kzre.krro.brush.protocol :as p]
            [top.kzre.krro.brush.util :as util]))

(defrecord DefaultMixer []
  p/IColorMixer
  (mix-colors [_ fg bg opacity blend-mode]
    ;; 简单 over 混合，忽略 blend-mode
    (let [fg-rgb (pop fg) bg-rgb (pop bg)
          fg-a   (peek fg) bg-a   (peek bg)
          a-out  (+ fg-a (* bg-a (- 1.0 (* opacity fg-a))))
          inv-out (if (zero? a-out) 0.0 (/ 1.0 a-out))]
      (if (zero? a-out)
        [0.0 0.0 0.0 0.0]
        [(/ (+ (* (nth fg-rgb 0) fg-a) (* (nth bg-rgb 0) bg-a (- 1.0 (* opacity fg-a)))) a-out)
         (/ (+ (* (nth fg-rgb 1) fg-a) (* (nth bg-rgb 1) bg-a (- 1.0 (* opacity fg-a)))) a-out)
         (/ (+ (* (nth fg-rgb 2) fg-a) (* (nth bg-rgb 2) bg-a (- 1.0 (* opacity fg-a)))) a-out)
         a-out])))
  (mix-pigments [_ pigment-keys ratios]
    ;; 默认不支持颜料混合，直接返回黑色
    [0.0 0.0 0.0]))

(def default-mixer (->DefaultMixer))