(ns top.kzre.krro.brush.mix
  "混合模式模块：根据 mix-mode 动态计算前景色，并维护状态。
   颜色均为 float[]。完全基于 TiledCanvas。"
  (:require [top.kzre.krro.brush.util :as util])
  (:import (java.util.function Consumer)
           (top.kzre.colorutils.color RGB)
           (top.kzre.krro.brush Mix)
           (top.kzre.krro.util.tile TiledCanvas)
           (java.util HashMap)))

(defn- sample-gaussian
  "从 TiledCanvas 中采样，cx, cy 为世界坐标，radius 为采样半径。"
  [^TiledCanvas canvas cx cy radius]
  (let [tile-size (.getTileSize canvas)
        ;; 读取瓦片映射（只读引用）
        tiles-map (HashMap.)]
    (.readTiles canvas
                (reify Consumer
                  (accept [_ m] (.putAll tiles-map m))))
    (Mix/sampleGaussianTiled tiles-map tile-size
                             (float cx) (float cy) (float radius))))

(defmulti mix
          (fn [brush-spec _params _state _canvas] (:mix-mode brush-spec)))

(defmethod mix :default
  [{:keys [color]} _params state _canvas]
  [color state])

;; 颜色携带
(defmethod mix :colored-brush
  [brush-spec params state canvas]
  (let [color           (:color brush-spec)
        blend-ratio     (float (or (util/param :blend-ratio params brush-spec) 0.5))
        carry-decay     (float (or (util/param :carry-decay params brush-spec) 0.5))
        decay-exponent  (float (or (util/param :decay-exponent params brush-spec) 1.0))
        min-carry       (float (or (util/param :min-carry params brush-spec) 0.0))
        carry           (or (:carry-color state) (RGB/rgbaToRgb color))
        bg-rgba         (sample-gaussian canvas (:x params) (:y params) 0.0)
        fg-rgb          (RGB/rgbaToRgb color)
        bg-rgb          (RGB/rgbaToRgb bg-rgba)
        blended         (RGB/mix fg-rgb bg-rgb blend-ratio)
        effective-decay (float (Math/pow carry-decay decay-exponent))
        clamped-decay   (max min-carry effective-decay)
        final-rgb       (RGB/mix carry blended clamped-decay)
        final-rgba      (RGB/withAlpha final-rgb (RGB/alpha color))]
    [final-rgba (assoc state :carry-color final-rgb)]))

(defmethod mix :dulling
  [brush-spec params state canvas]
  (let [color           (:color brush-spec)
        dulling-ratio   (float (util/param :dulling-ratio params brush-spec 0.5))
        dulling-opacity (float (util/param :dulling-opacity params brush-spec 1.0))
        bg-rgba         (sample-gaussian canvas (:x params) (:y params) 0.0)
        fg-rgb          (RGB/rgbaToRgb color)
        bg-rgb          (RGB/rgbaToRgb bg-rgba)
        dulled-rgb      (RGB/mix bg-rgb fg-rgb dulling-ratio)
        final-alpha     (* (RGB/alpha color) dulling-opacity)
        final-color     (RGB/withAlpha dulled-rgb final-alpha)]
    [final-color state]))

(defmethod mix :smudge
  [brush-spec params state canvas]
  (let [color           (:color brush-spec)
        last-params     (:last-params state)
        smudge-radius   (float (or (util/param :smudge-radius params brush-spec) 10.0))
        smudge-strength (float (or (util/param :smudge-strength params brush-spec) 0.5))
        sample-x        (float (util/param :x last-params params))
        sample-y        (float (util/param :y last-params params))
        smudge-src      (sample-gaussian canvas sample-x sample-y smudge-radius)
        fg-rgb          (RGB/rgbaToRgb color)
        src-rgb         (RGB/rgbaToRgb smudge-src)
        mixed           (RGB/mix fg-rgb src-rgb smudge-strength)
        final-color     (RGB/withAlpha mixed (RGB/alpha color))]
    [final-color (assoc state :last-params params)]))