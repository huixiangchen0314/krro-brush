(ns top.kzre.krro.brush.post
  "后处理器：基于 color-utils 的 double[] 颜色。
   直接操作画布数组 data。"
  (:require [top.kzre.krro.brush.util :as util])
  (:import [top.kzre.colorutils.color RGB]))

(defmulti apply-post-effect
          (fn [effect _pixel _data _w _h _dab-mask _brush-spec _params] (:type effect)))

;; ── 水彩边缘效果 ──────────────────────────────────────
(defmethod apply-post-effect :watercolor-edge
  [effect pixel _data _w _h dab-mask _brush-spec params]
  (let [intensity (or (:intensity effect) 0.5)
        alpha     (RGB/alpha pixel)          ;; double
        px        (:x params)
        py        (:y params)]
    (if (and dab-mask (> alpha 0.01) (< alpha 0.99))
      (let [w (:width dab-mask) h (:height dab-mask)
            data (:data dab-mask)
            left   (if (> px 0) (aget data (+ (dec px) (* py w))) alpha)
            right  (if (< px (dec w)) (aget data (+ (inc px) (* py w))) alpha)
            up     (if (> py 0) (aget data (+ px (* (dec py) w))) alpha)
            down   (if (< py (dec h)) (aget data (+ px (* (inc py) w))) alpha)
            grad (Math/sqrt (+ (Math/pow (- right left) 2)
                               (Math/pow (- down up) 2)))
            raw-factor (* grad 5.0 intensity alpha)
            edge-factor (util/clamp01 raw-factor)
            ;; 当前 pixel 是 double[4]
            r (aget pixel 0)
            g (aget pixel 1)
            b (aget pixel 2)
            ;; 向黑色混合
            darken (fn [c] (* c (- 1.0 edge-factor)))]
        (RGB/rgba (darken r) (darken g) (darken b) alpha))
      pixel)))

;; ── 纸纹合成效果 ──────────────────────────────────────
(defmethod apply-post-effect :paper-texture
  [effect pixel _data _w _h _dab-mask brush-spec params]
  (let [strength (or (:strength effect) 0.2)
        paper-texture (util/param :texture effect brush-spec)
        x (:x params)
        y (:y params)
        paper (or paper-texture {:width 0 :height 0 :data (double-array 0)})
        pw (:width paper) ph (:height paper)
        pdata (:data paper)
        tx (mod x pw)
        ty (mod y ph)
        tex-val (if (and (>= tx 0) (< tx pw) (>= ty 0) (< ty ph))
                  (aget pdata (+ tx (* ty pw)))
                  0.5)
        factor (- 1.0 (* strength tex-val))
        r (aget pixel 0)
        g (aget pixel 1)
        b (aget pixel 2)
        new-r (util/clamp01 (* r factor))
        new-g (util/clamp01 (* g factor))
        new-b (util/clamp01 (* b factor))]
    (RGB/rgba new-r new-g new-b (RGB/alpha pixel))))

;; ── 默认直通 ──────────────────────────────────────────
(defmethod apply-post-effect :default
  [_effect pixel _data _w _h _dab-mask _brush-spec _params]
  pixel)

;; ── 管线驱动 ──────────────────────────────────────────
(defn apply-post-pipeline
  "按顺序应用后处理效果。
   pixel     - 当前 RGBA 数组 double[4]
   data      - 画布数组
   w         - 画布宽度
   h         - 画布高度
   dab-mask  - dab 遮罩 {:width :height :data}
   brush-spec - 笔刷定义
   params    - 当前 dab 参数，包含 :x, :y"
  [pixel data w h dab-mask brush-spec params]
  (let [post-spec (:post-spec brush-spec)]
    (reduce (fn [pixel' effect]
              (apply-post-effect effect pixel' data w h dab-mask brush-spec params))
            pixel
            post-spec)))