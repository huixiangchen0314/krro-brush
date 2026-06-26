(ns top.kzre.krro.brush.post
  "后处理器：水彩边缘、纸纹合成等辅助函数。"
  (:require [top.kzre.krro.brush.util :as util]))

(defmulti apply-post-effect
          "对单个像素应用一种后处理效果。分发 key 为效果名。"
          (fn [effect-name pixel dab-mask px py alpha intensity paper paper-x paper-y strength]
            effect-name))

(defmethod apply-post-effect :watercolor-edge
  [effect pixel dab-mask px py alpha intensity _ _ _ _]
  (if (and (> alpha 0.01) (< alpha 0.99))
    (let [w (:width dab-mask) h (:height dab-mask)
          data (:data dab-mask)
          left   (if (> px 0) (aget data (+ (dec px) (* py w))) alpha)
          right  (if (< px (dec w)) (aget data (+ (inc px) (* py w))) alpha)
          up     (if (> py 0) (aget data (+ px (* (dec py) w))) alpha)
          down   (if (< py (dec h)) (aget data (+ px (* (inc py) w))) alpha)
          grad (Math/sqrt (+ (Math/pow (- right left) 2) (Math/pow (- down up) 2)))
          edge-factor (util/clamp 0.0 1.0 (* grad 5.0 intensity))
          r (nth pixel 0) g (nth pixel 1) b (nth pixel 2) a (nth pixel 3)
          darkened (mapv #(util/lerp % 0.0 edge-factor) [r g b])]
      (conj (vec darkened) a))
    pixel))

(defmethod apply-post-effect :paper-texture
  [_ pixel _ _ _ _ _ paper paper-x paper-y strength]
  (let [pw (:width paper) ph (:height paper)
        pdata (:data paper)
        tx (mod paper-x pw)
        ty (mod paper-y ph)
        tex-alpha (if (and (>= tx 0) (< tx pw) (>= ty 0) (< ty ph))
                    (aget pdata (+ tx (* ty pw)))
                    0.5)
        factor (- 1.0 (* strength tex-alpha))]
    (mapv #(util/clamp 0.0 1.0 (* % factor)) pixel)))

(defn apply-watercolor-edge [pixel dab-mask px py alpha intensity]
  (apply-post-effect :watercolor-edge pixel dab-mask px py alpha intensity nil nil nil nil))

(defn apply-paper-texture [pixel paper canvas-x canvas-y strength]
  (apply-post-effect :paper-texture pixel nil 0 0 0.0 nil paper canvas-x canvas-y strength))