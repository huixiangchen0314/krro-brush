(ns top.kzre.krro.brush.stroke
  "笔触构建与渲染：完全基于瓦片画布，不再支持全幅数组。
   集成 mix 模块，支持动态混色。颜色全部为 float[]。"
  (:require
    [taoensso.tufte :refer [profile]]
    [top.kzre.krro.brush.dab :as dab]
    [top.kzre.krro.brush.dynamics :as dynamics]
    [top.kzre.krro.brush.event :as event]
    [top.kzre.krro.brush.measure :as measure]
    [top.kzre.krro.brush.default :as default]
    [top.kzre.krro.brush.mix :as mix]
    [top.kzre.krro.brush.post :as post]
    [top.kzre.krro.brush.resample :as resample]
    [top.kzre.krro.brush.smooth :as smooth]
    [top.kzre.krro.brush.taper :as taper]
    [top.kzre.krro.brush.util :as util]
    [top.kzre.krro.util.tiled-canvas :as tcanvas])
  (:import
   (java.util HashMap)
   (top.kzre.colorutils.blend Blends)
   (top.kzre.krro.brush PixelPostprocessor Stroke)))


(defn events->stroke
  [brush-spec events & {:keys [skip-smooth?] :as opts
                        :or {skip-smooth? false}}]
  (let [filled    (map event/polyfill events)
        smoothed  (if skip-smooth? filled (smooth/smooth filled (:smooth brush-spec)))
        resampled (resample/resample smoothed brush-spec)
        measured  (measure/measure resampled)                       ;; 附加 :length
        defaulted (default/apply-defaults measured brush-spec)        ;; 确保 :radius 等存在
        mapped    (mapv #(dynamics/map-dynamics % (:dynamics brush-spec)) defaulted)
        stroke    {:brush brush-spec :params mapped}
        ;; 若 brush-spec 包含锥化参数，则自动应用锥化
        tapered   (if (or (:taper-start-px brush-spec) (:taper-end-px brush-spec)
                          (:taper-start-ratio brush-spec) (:taper-end-ratio brush-spec))
                    (taper/taper-stroke stroke
                                        :taper-start-px (:taper-start-px brush-spec)
                                        :taper-end-px (:taper-end-px brush-spec)
                                        :taper-start-ratio (:taper-start-ratio brush-spec)
                                        :taper-end-ratio (:taper-end-ratio brush-spec)
                                        :taper-fields (:taper-fields brush-spec))
                    stroke)]
    tapered))


(defn join-stroke
  [& strokes]
  (when (seq strokes)
    (let [first-stroke (first strokes)
          brush        (:brush first-stroke)]
      {:brush  brush
       :params (vec (apply concat (map :params strokes)))})))

;; ── 渲染辅助 ───────────────────────────────────────
(defn- preprocess-params [params]
  (-> params
      (update :x #(int (Math/round (double %))))
      (update :y #(int (Math/round (double %))))))

(defn make-post-processor
  "创建后处理器，仅处理当前像素（不依赖画布数组）。
   如需水彩边缘等需要邻居像素的效果，需额外传入瓦片访问器，此处暂不实现。"
  [dab-mask brush-spec params]
  (reify PixelPostprocessor
    (process [_ pixel]
      ;; 调用后处理管线，data/w/h 传 nil/0 因为后处理不再依赖它们
      (post/apply-post-pipeline pixel nil 0 0 dab-mask brush-spec params))))

;; ── 将 canvas 的 tiles 转换为可变 HashMap，并可能在调用后重建 canvas ──
(defn- with-mutable-tiles
  "将 canvas 的 :tiles 转为 HashMap，调用 f，根据修改后的 map 重建 canvas。
   f 接收 HashMap 并应原地修改它。返回新的 canvas。"
  [canvas f]
  (let [mutable-tiles (HashMap. (:tiles canvas))
        _ (f mutable-tiles)
        ;; 根据 mutable-tiles 的键重新计算索引范围，避免遗漏扩展的 tile
        new-min-tx (reduce (fn [acc k]
                             (let [tx (tcanvas/unpack-tx k)]
                               (if (< tx acc) tx acc))) Long/MAX_VALUE (keys mutable-tiles))
        new-max-tx (reduce (fn [acc k]
                             (let [tx (tcanvas/unpack-tx k)]
                               (if (> tx acc) tx acc))) Long/MIN_VALUE (keys mutable-tiles))
        new-min-ty (reduce (fn [acc k]
                             (let [ty (tcanvas/unpack-ty k)]
                               (if (< ty acc) ty acc))) Long/MAX_VALUE (keys mutable-tiles))
        new-max-ty (reduce (fn [acc k]
                             (let [ty (tcanvas/unpack-ty k)]
                               (if (> ty acc) ty acc))) Long/MIN_VALUE (keys mutable-tiles))]
    (cond-> canvas
            true (assoc :tiles (into {} mutable-tiles))
            ;; 仅当有瓦片时更新范围，否则保持原有范围不变
            (not (empty? mutable-tiles))
            (assoc :min-tx (int new-min-tx)
                   :max-tx (int new-max-tx)
                   :min-ty (int new-min-ty)
                   :max-ty (int new-max-ty)))))

;; ── 单个 dab 渲染，返回 [new-canvas, dirty-tiles-set] ──
(defn- render-dab!
  [canvas brush params fg-color]
  (profile {:id ::dab}
           (let [params'        (preprocess-params params)
                 dab-mask       (dab/generate-dab brush params')
                 blend-mode     (util/blend-mode-str brush Blends/NORMAL)
                 extra-opacity  (float (or (util/param :opacity params' brush) 1.0))
                 x              (int (:x params'))
                 y              (int (:y params'))
                 mw             (int (:width dab-mask))
                 mh             (int (:height dab-mask))
                 ^floats mask-data (:data dab-mask)
                 post-processor (make-post-processor dab-mask brush params')
                 result (atom nil)]
             ;; 使用 with-mutable-tiles 保证 Java 层可写并捕获返回的脏集合
             (let [new-canvas (with-mutable-tiles canvas
                                                  (fn [mutable-tiles]
                                                    (let [dirty (Stroke/stampDabTiled mutable-tiles
                                                                                      (int (:tile-size canvas))
                                                                                      mask-data mw mh x y
                                                                                      fg-color blend-mode extra-opacity
                                                                                      post-processor)]
                                                      (reset! result dirty))))]
               [new-canvas (set @result)]))))

;; ── 笔触级渲染，返回 [new-canvas, dirty-tiles-set] ──
(defn render-stroke-dirties!
  [canvas {:keys [brush params] :as _stroke}]
  (reduce (fn [[c dirty] point-params]
            (let [[fg-color new-state] (mix/mix brush point-params {} c)
                  [new-c dab-dirty] (render-dab! c brush point-params fg-color)]
              [new-c (if dab-dirty (into dirty dab-dirty) dirty)]))
          [canvas #{}]
          params))

(defn render-stroke!
  "渲染整个笔触，返回 [new-canvas, dirty-tiles-set]。"
  [canvas stroke]
  (render-stroke-dirties! canvas stroke))