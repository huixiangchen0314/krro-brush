(ns top.kzre.krro.brush.batch.dab-renderer
  "基于数组批量操作的 Dab 渲染器，实现 IDabRenderer 协议。
   如果画布支持直接原始数组访问，则使用高效的数组操作；
   否则回退到逐像素的 ICanvas 方法。"
  (:require [top.kzre.krro.brush.protocol :as p]
            [top.kzre.krro.brush.post :as post]
            [top.kzre.krro.brush.util :as util]))

;; ── 内部协议：支持原始数组的画布 ──────────────────────
(defprotocol IRawArrayCanvas
  (get-raw-pixels [this] "返回 [width, height, double-array RGBA...]"))

;; ── 批量混合核心：同时处理混合与后处理 ────────────────
(defn- batch-blend-and-post
  [^doubles dst w h cx cy dab-mask fg-color opacity mix-mode mixer post-spec paper-texture]
  (let [mw (:width dab-mask) mh (:height dab-mask)
        mask-data (:data dab-mask)
        half-w (quot mw 2)
        half-h (quot mh 2)
        water-edge? (get-in post-spec [:watercolor :enable] false)
        edge-intensity (get-in post-spec [:watercolor :intensity] 0.5)
        paper-enabled? (and paper-texture (get-in post-spec [:paper :enable] false))
        paper-strength (get-in post-spec [:paper :strength] 0.2)
        ;; 脏矩形
        dirty-x (volatile! Integer/MAX_VALUE) dirty-y (volatile! Integer/MAX_VALUE)
        dirty-max-x (volatile! Integer/MIN_VALUE) dirty-max-y (volatile! Integer/MIN_VALUE)]
    (dotimes [py mh]
      (dotimes [px mw]
        (let [alpha (aget mask-data (+ px (* py mw)))]
          (when (> alpha 0.0)
            (let [canvas-x (- cx half-w px)
                  canvas-y (- cy half-h py)]
              (when (and (>= canvas-x 0) (< canvas-x w)
                         (>= canvas-y 0) (< canvas-y h))
                (let [idx (* 4 (+ canvas-x (* canvas-y w)))
                      bg-r (aget dst idx) bg-g (aget dst (inc idx)) bg-b (aget dst (+ idx 2)) bg-a (aget dst (+ idx 3))
                      ;; 直接内联混合，避免协议调用
                      mixed (p/mix-colors mixer fg-color [bg-r bg-g bg-b bg-a] (* opacity alpha) mix-mode)
                      mixed (cond-> mixed
                                    water-edge?
                                    (post/apply-watercolor-edge dab-mask px py alpha edge-intensity)
                                    paper-enabled?
                                    (post/apply-paper-texture paper-texture canvas-x canvas-y paper-strength))]
                  (aset dst idx (double (first mixed)))
                  (aset dst (inc idx) (double (second mixed)))
                  (aset dst (+ idx 2) (double (nth mixed 2)))
                  (aset dst (+ idx 3) (double (nth mixed 3)))
                  ;; 更新脏矩形
                  (vswap! dirty-x min canvas-x) (vswap! dirty-y min canvas-y)
                  (vswap! dirty-max-x max (inc canvas-x)) (vswap! dirty-max-y max (inc canvas-y)))))))))
    ;; 返回脏矩形
    (let [dx @dirty-x dy @dirty-y]
      (if (and (< dx Integer/MAX_VALUE) (< dy Integer/MAX_VALUE))
        [dx dy (- @dirty-max-x dx) (- @dirty-max-y dy)]
        nil))))

;; ── 批量渲染器实现 ──────────────────────────────────
(defrecord BatchDabRenderer []
  p/IDabRenderer
  (render-dab [this canvas dab-mask fg-color opacity mix-mode mixer post-spec paper-texture]
    (let [w (p/width canvas) h (p/height canvas)]
      (if (satisfies? IRawArrayCanvas canvas)
        ;; 批量路径：直接操作底层数组
        (let [[_ _ dst] (get-raw-pixels canvas)
              dirty (batch-blend-and-post dst w h (:cx dab-mask) (:cy dab-mask)
                                          dab-mask fg-color opacity mix-mode mixer post-spec paper-texture)]
          {:canvas canvas :dirty-rect dirty})
        ;; 降级：逐像素操作，保持兼容
        (let [cx (:cx dab-mask) cy (:cy dab-mask)
              mw (:width dab-mask) mh (:height dab-mask)
              mask-data (:data dab-mask)
              half-w (quot mw 2) half-h (quot mh 2)
              water-edge? (get-in post-spec [:watercolor :enable] false)
              edge-intensity (get-in post-spec [:watercolor :intensity] 0.5)
              paper-enabled? (and paper-texture (get-in post-spec [:paper :enable] false))
              paper-strength (get-in post-spec [:paper :strength] 0.2)
              dirty-x (volatile! Integer/MAX_VALUE) dirty-y (volatile! Integer/MAX_VALUE)
              dirty-max-x (volatile! Integer/MIN_VALUE) dirty-max-y (volatile! Integer/MIN_VALUE)
              canvas-atom (atom canvas)]
          (doseq [py (range mh) px (range mw)
                  :let [alpha (aget mask-data (+ px (* py mw)))]
                  :when (> alpha 0.0)
                  :let [canvas-x (- cx half-w px) canvas-y (- cy half-h py)]
                  :when (and (>= canvas-x 0) (< canvas-x w)
                             (>= canvas-y 0) (< canvas-y h))]
            (let [bg (p/get-pixel @canvas-atom canvas-x canvas-y)
                  mixed (p/mix-colors mixer fg-color bg (* opacity alpha) mix-mode)
                  final (cond-> mixed
                                water-edge? (post/apply-watercolor-edge dab-mask px py alpha edge-intensity)
                                paper-enabled? (post/apply-paper-texture paper-texture canvas-x canvas-y paper-strength))
                  new-canvas (p/set-pixel! @canvas-atom canvas-x canvas-y final)]
              (reset! canvas-atom new-canvas)
              (vswap! dirty-x min canvas-x) (vswap! dirty-y min canvas-y)
              (vswap! dirty-max-x max (inc canvas-x)) (vswap! dirty-max-y max (inc canvas-y))))
          (let [dx @dirty-x dy @dirty-y
                dirty (when (< dx Integer/MAX_VALUE) [dx dy (- @dirty-max-x dx) (- @dirty-max-y dy)])]
            {:canvas @canvas-atom :dirty-rect dirty}))))))

(def default-batch-dab-renderer (->BatchDabRenderer))