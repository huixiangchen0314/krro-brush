(ns top.kzre.krro.brush.stroke
  "笔触构建与渲染：从事件生成笔触数据，并不可变地渲染到数组。
   集成 mix 模块，支持动态混色（colored-brush、dulling、smudge 等）。
   颜色全部为 double[]，使用 color-utils 和 Blends。"
  (:require
    [taoensso.tufte :refer [p profile]]
    [top.kzre.krro.brush.dab :as dab]
    [top.kzre.krro.brush.dynamics :as dynamics]
    [top.kzre.krro.brush.event :as event]
    [top.kzre.krro.brush.mix :as mix]
    [top.kzre.krro.brush.post :as post]
    [top.kzre.krro.brush.rect :as rect]
    [top.kzre.krro.brush.resample :as resample]
    [top.kzre.krro.brush.smooth :as smooth]
    [top.kzre.krro.brush.util :as util])
  (:import [top.kzre.colorutils.color RGB]
           [top.kzre.colorutils.blend Blends]
           (top.kzre.krro.brush PixelPostprocessor Stroke)))

;; ── 笔触构建（未改动） ──────────────────────────────
(defn events->stroke
  [brush-spec events spacing radius]
  (let [filled    (map event/polyfill events)
        smoothed  (smooth/smooth filled (:smooth brush-spec))
        resampled (resample/resample smoothed spacing radius)
        mapped    (mapv #(dynamics/map-dynamics % (:dynamics brush-spec)) resampled)]
    {:brush  brush-spec
     :params mapped}))

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
  [data w h dab-mask brush-spec params]
  (reify PixelPostprocessor
    (process [_ pixel]
      (let [p (post/apply-post-pipeline pixel data w h dab-mask brush-spec params) ]
        p))))

(defn- render-dab!
  [^doubles dst w h brush params fg-color]
  (profile {:id ::dab}
           (let [w (long w) h (long h)

                 params'      (preprocess-params params)
                 dab-mask     (dab/generate-dab brush params')
                 blend-mode   (util/blend-mode-str brush Blends/NORMAL)
                 extra-opacity (double (or (util/param :opacity params' brush) 1.0))
                 x            (long (:x params'))
                 y            (long (:y params'))
                 mw           (long (:width dab-mask))
                 mh           (long (:height dab-mask))
                 mask-data    ^doubles (:data dab-mask)
                 post-processor (make-post-processor dst w h dab-mask brush params')
                 result       (Stroke/stampDab dst (int w) (int h)
                                               mask-data (int mw) (int mh)
                                               (int x) (int y)
                                               fg-color blend-mode extra-opacity
                                               post-processor)]
             (when result
               (rect/make-rect (int (aget result 0))
                               (int (aget result 1))
                               (int (aget result 2))
                               (int (aget result 3)))))))


  (defn render-stroke!
    "将笔触渲染到给定的 data 数组上（原地修改）。
     返回合并的脏矩形。"
    [^doubles data w h {:keys [brush params] :as _stroke}]
    (let [[dirties _final-state]
          (reduce (fn [[dirties state] point-params]
                    (let [[fg-color new-state] (mix/mix brush point-params state data w h)
                          dirty (render-dab! data w h brush point-params fg-color)]
                      [(if dirty (conj dirties dirty) dirties)
                       new-state]))
                  [[] {}]
                  params)]
      (apply rect/merge-rects dirties)))