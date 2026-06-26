(ns top.kzre.krro.brush.post
  "后处理器：水彩边缘、纸纹合成等辅助函数。"
  (:require [top.kzre.krro.brush.util :as util]))

;; ── 多方法：根据 effect 分发 ──────────────────────────
(defmulti apply-post-effect
          "对单个像素应用一种后处理效果。
           effect    - 效果关键字 :watercolor-edge 或 :paper-texture
           options   - map，包含该效果需要的所有参数，见各实现方法的 docstring。"
          (fn [effect options] effect))

;; ── 水彩边缘效果 ──────────────────────────────────────
(defmethod apply-post-effect :watercolor-edge
  [_ opts]
  (let [{:keys [pixel dab-mask px py alpha intensity]} opts]
    (if (and (> alpha 0.01) (< alpha 0.99))
      (let [w (:width dab-mask) h (:height dab-mask)
            data (:data dab-mask)
            left   (if (> px 0) (aget data (+ (dec px) (* py w))) alpha)
            right  (if (< px (dec w)) (aget data (+ (inc px) (* py w))) alpha)
            up     (if (> py 0) (aget data (+ px (* (dec py) w))) alpha)
            down   (if (< py (dec h)) (aget data (+ px (* (inc py) w))) alpha)
            grad (Math/sqrt (+ (Math/pow (- right left) 2) (Math/pow (- down up) 2)))
            raw-factor (* grad 5.0 intensity alpha)
            edge-factor (util/clamp 0.0 1.0 raw-factor)
            [r g b a] pixel
            darkened (mapv #(util/lerp % 0.0 edge-factor) [r g b])]
        (conj (vec darkened) a))
      pixel)))

;; ── 纸纹合成效果 ──────────────────────────────────────
(defmethod apply-post-effect :paper-texture
  [_ opts]
  (let [{:keys [pixel paper paper-x paper-y strength]} opts
        pw (:width paper) ph (:height paper)
        pdata (:data paper)
        tx (mod paper-x pw)
        ty (mod paper-y ph)
        tex-alpha (if (and (>= tx 0) (< tx pw) (>= ty 0) (< ty ph))
                    (aget pdata (+ tx (* ty pw)))
                    0.5)
        factor (- 1.0 (* strength tex-alpha))
        rgb (mapv #(util/clamp 0.0 1.0 (* % factor)) (subvec pixel 0 3))
        a (peek pixel)]
    (conj rgb a)))

;; ── 便捷函数（保持原有调用方式）───────────────────────
(defn apply-watercolor-edge
  "应用水彩边缘效果。直接调用 apply-post-effect。"
  [pixel dab-mask px py alpha intensity]
  (apply-post-effect :watercolor-edge
                     {:pixel pixel :dab-mask dab-mask
                      :px px :py py :alpha alpha
                      :intensity intensity}))

(defn apply-paper-texture
  "应用纸纹纹理。直接调用 apply-post-effect。"
  [pixel paper canvas-x canvas-y strength]
  (apply-post-effect :paper-texture
                     {:pixel pixel :paper paper
                      :paper-x canvas-x :paper-y canvas-y
                      :strength strength}))