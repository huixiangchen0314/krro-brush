(ns top.kzre.krro.brush.stroke
  "笔触构建与渲染：从事件生成笔触数据，并不可变地渲染到数组。
   集成 mix 模块，支持动态混色（colored-brush、dulling、smudge 等）。
   颜色全部为 float[]，使用 color-utils 和 Blends。"
  (:require
    [taoensso.tufte :refer [profile]]
    [top.kzre.krro.brush.dab :as dab]
    [top.kzre.krro.brush.dynamics :as dynamics]
    [top.kzre.krro.brush.event :as event]
    [top.kzre.krro.brush.mix :as mix]
    [top.kzre.krro.brush.post :as post]
    [top.kzre.krro.brush.rect :as rect]
    [top.kzre.krro.brush.resample :as resample]
    [top.kzre.krro.brush.smooth :as smooth]
    [top.kzre.krro.brush.util :as util])
  (:import (top.kzre.colorutils.blend Blends)
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
      ;; pixel 是 float[4]，后处理管线现在也操作 float[]
      (post/apply-post-pipeline pixel data w h dab-mask brush-spec params))))

(defn- render-dab!
  "将单个 dab 渲染到画布，返回脏矩形。data 为 float-array。"
  [^floats dst w h brush params fg-color]
  (profile {:id ::dab}
           (let [w (int w) h (int h)
                 params'      (preprocess-params params)
                 dab-mask     (dab/generate-dab brush params')
                 blend-mode   (util/blend-mode-str brush Blends/NORMAL)
                 extra-opacity (float (or (util/param :opacity params' brush) 1.0))
                 x            (int (:x params'))
                 y            (int (:y params'))
                 mw           (int (:width dab-mask))
                 mh           (int (:height dab-mask))
                 ^floats mask-data (:data dab-mask)
                 post-processor (make-post-processor dst w h dab-mask brush params')
                 result       (Stroke/stampDab dst w h
                                               mask-data mw mh
                                               x y
                                               fg-color blend-mode extra-opacity
                                               post-processor)]
             (when result
               (rect/make-rect (int (aget result 0))
                               (int (aget result 1))
                               (int (aget result 2))
                               (int (aget result 3)))))))

(defn render-stroke-dirties!
  "将笔触渲染到 data 数组上（原地修改），返回脏矩形序列（未合并）。"
  [^floats data w h {:keys [brush params] :as _stroke}]
  (let [[dirties _final-state]
        (reduce (fn [[dirties state] point-params]
                  (let [[fg-color new-state] (mix/mix brush point-params state data w h)
                        dirty (render-dab! data w h brush point-params fg-color)]
                    [(if dirty (conj dirties dirty) dirties)
                     new-state]))
                [[] {}]
                params)]
    dirties))

(defn render-stroke!
  [^floats data w h stroke]
  (let [dirties (render-stroke-dirties! data w h stroke)]
    (apply rect/merge-rects dirties)))