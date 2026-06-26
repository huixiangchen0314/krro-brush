(ns top.kzre.krro.brush.mix
  "内置基础混色器（测试用），仅提供简单的前景色覆盖，无 krro‑color 集成。
   真正的颜色混合应由宿主通过 IColorMixer 协议注入。"
  (:require [top.kzre.krro.brush.util :as util]))

(defn default-mix-colors
  "简单前景色混合：忽略混合模式，仅按不透明度混合前景与背景。
   返回新颜色 [r g b a]。"
  [fg bg opacity blend-mode]
  (let [fg-rgb (pop fg)
        bg-rgb (pop bg)
        fg-a   (peek fg)
        bg-a   (peek bg)
        ;; 简单 over 混合
        a-out (+ fg-a (* bg-a (- 1.0 (* opacity fg-a))))
        inv-out (if (zero? a-out) 0.0 (/ 1.0 a-out))]
    (if (zero? a-out)
      [0.0 0.0 0.0 0.0]
      [(/ (+ (* (nth fg-rgb 0) fg-a) (* (nth bg-rgb 0) bg-a (- 1.0 (* opacity fg-a)))) a-out)
       (/ (+ (* (nth fg-rgb 1) fg-a) (* (nth bg-rgb 1) bg-a (- 1.0 (* opacity fg-a)))) a-out)
       (/ (+ (* (nth fg-rgb 2) fg-a) (* (nth bg-rgb 2) bg-a (- 1.0 (* opacity fg-a)))) a-out)
       a-out])))