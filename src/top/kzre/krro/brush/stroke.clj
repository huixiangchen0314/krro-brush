(ns top.kzre.krro.brush.stroke
  "笔触构建与渲染：完全基于 TiledCanvas。集成 mix 模块，支持动态混色。"
  (:require
    [taoensso.tufte :as p]
    [top.kzre.krro.brush.dab :as dab]
    [top.kzre.krro.brush.dynamics :as dynamics]
    [top.kzre.krro.brush.event :as event]
    [top.kzre.krro.brush.measure :as measure]
    [top.kzre.krro.brush.mix :as mix]
    [top.kzre.krro.brush.post :as post]
    [top.kzre.krro.brush.resample :as resample]
    [top.kzre.krro.brush.smooth :as smooth]
    [top.kzre.krro.brush.taper :as taper]
    [top.kzre.krro.brush.util :as util])
  (:import
    (top.kzre.colorutils.blend Blends)
    (top.kzre.krro.brush PixelPostprocessor Stroke)
    (top.kzre.krro.util.tile Canvas TiledCanvas)))

;; ── 事件→笔触构建（保持不变）─────────────────────
(defn events->stroke
  [brush-spec events & {:keys [skip-smooth?] :as opts
                        :or {skip-smooth? false}}]
  (p/profile {:id :krro.brush/events->stroke
              :data {:event-count (count events)
                     :brush-id (:id brush-spec)}}
             (let [filled    (p/p :polyfill (map event/polyfill events))
                   smoothed  (if skip-smooth?
                               filled
                               (p/p :smooth (smooth/smooth filled (:smooth brush-spec))))
                   resampled (p/p :resample (resample/resample smoothed brush-spec))
                   measured  (p/p :measure (measure/measure resampled))
                   mapped    (p/p :map-dynamics
                                  (mapv #(dynamics/map-dynamics % brush-spec) measured))
                   stroke    {:brush brush-spec :params mapped}
                   tapered   (if (or (:taper-start-px brush-spec) (:taper-end-px brush-spec)
                                     (:taper-start-ratio brush-spec) (:taper-end-ratio brush-spec))
                               (p/p :taper
                                    (taper/taper-stroke stroke
                                                        :taper-start-px (:taper-start-px brush-spec)
                                                        :taper-end-px (:taper-end-px brush-spec)
                                                        :taper-start-ratio (:taper-start-ratio brush-spec)
                                                        :taper-end-ratio (:taper-end-ratio brush-spec)
                                                        :taper-fields (:taper-fields brush-spec)))
                               stroke)]
               tapered)))

(defn join-stroke
  [& strokes]
  (when (seq strokes)
    (let [first-stroke (first strokes)
          brush        (:brush first-stroke)]
      {:brush  brush
       :params (vec (apply concat (map :params strokes)))})))

;; ── 渲染辅助 ─────────────────────────────────────
(defn- preprocess-params [params]
  (-> params
      (update :x #(int (Math/round (double %))))
      (update :y #(int (Math/round (double %))))))

(defn make-post-processor
  "创建后处理器，仅处理当前像素（不依赖画布数组）。"
  [dab-mask brush-spec params]
  (reify PixelPostprocessor
    (process [_ pixel]
      (post/apply-post-pipeline pixel nil 0 0 dab-mask brush-spec params))))

;; ── 单个 dab 渲染（直接操作 TiledCanvas） ─────────
(defn- render-dab!
  [^Canvas canvas brush params fg-color]
  (p/profile {:id :krro.brush/render-dab}
             (let [params'        (preprocess-params params)
                   dab-mask       (p/p :generate-dab (dab/generate-dab brush params'))
                   blend-mode     (util/blend-mode-str brush Blends/NORMAL)
                   extra-opacity  (float (or (util/param :opacity params' brush) 1.0))
                   x              (int (:x params'))
                   y              (int (:y params'))
                   mw             (int (:width dab-mask))
                   mh             (int (:height dab-mask))
                   ^floats mask-data (:data dab-mask)
                   post-processor (make-post-processor dab-mask brush params')
                   ;; 直接调用 Stroke 的新方法，由 Java 侧处理拷贝与合并
                   dirty-set      (Stroke/stampDab canvas
                                                   mask-data mw mh x y
                                                   fg-color blend-mode extra-opacity
                                                   post-processor)]
               [canvas (set dirty-set)])))

;; ── 笔触级渲染 ─────────────────────────────────
(defn render-stroke-dirties!
  [canvas {:keys [brush params]}]
  (p/profile {:id :krro.brush/render-stroke}
             (p/p :render-total-stroke
                  (reduce (fn [[^TiledCanvas c dirty] point-params]
                            (p/p :render-single-event
                                 (let [[fg-color _] (p/p :mix-color (mix/mix brush point-params {} c))
                                       [new-c dab-dirty] (p/p :render-dab (render-dab! c brush point-params fg-color))]
                                   [new-c (into dirty dab-dirty)])))
                          [canvas #{}]
                          params))))

(defn render-stroke!
  "渲染整个笔触，返回 [new-canvas, dirty-tiles-set]。"
  [canvas stroke]
  (render-stroke-dirties! canvas stroke))