(ns top.kzre.krro.brush.mix
  "混合模式模块：根据 mix-mode 动态计算前景色，并维护状态。
   颜色均为 float[]。"
  (:require [top.kzre.krro.brush.util :as util])
  (:import (top.kzre.colorutils.color RGB)
           (top.kzre.krro.brush Mix)))

(defmulti mix
          (fn [brush-spec _params _state _data _w _h] (:mix-mode brush-spec)))

(defmethod mix :default
  [{:keys [color]} _params state _data _w _h]
  [color state])

(defmethod mix :colored-brush
  [brush-spec params state data w h]
  (let [color           (:color brush-spec)                      ;; float[4] RGBA
        blend-ratio     (float (util/param :blend-ratio params brush-spec 0.5))
        carry-decay     (float (util/param :carry-decay params brush-spec 0.5))
        decay-exponent  (float (util/param :decay-exponent params brush-spec 1.0))
        min-carry       (float (util/param :min-carry params brush-spec 0.0))
        carry           (or (:carry-color state)                 ;; float[3] RGB
                            (RGB/rgbaToRgb color))
        ;; 单点背景采样（radius=0）
        bg-rgba         (Mix/sampleGaussian data (int w) (int h)
                                            (float (:x params)) (float (:y params)) 0.0)
        fg-rgb          (RGB/rgbaToRgb color)
        bg-rgb          (RGB/rgbaToRgb bg-rgba)
        blended         (RGB/mix fg-rgb bg-rgb blend-ratio)      ;; float[3]
        effective-decay (float (Math/pow carry-decay decay-exponent))
        clamped-decay   (max min-carry effective-decay)
        final-rgb       (RGB/mix carry blended clamped-decay)    ;; float[3]
        final-rgba      (RGB/withAlpha final-rgb (RGB/alpha color))]
    [final-rgba (assoc state :carry-color final-rgb)]))

(defmethod mix :dulling
  [brush-spec params state data w h]
  (let [color           (:color brush-spec)                      ;; float[4] RGBA
        dulling-ratio   (float (util/param :dulling-ratio params brush-spec 0.5))
        dulling-opacity (float (util/param :dulling-opacity params brush-spec 1.0))
        bg-rgba         (Mix/sampleGaussian data (int w) (int h)
                                            (float (:x params)) (float (:y params)) 0.0)
        fg-rgb          (RGB/rgbaToRgb color)
        bg-rgb          (RGB/rgbaToRgb bg-rgba)
        dulled-rgb      (RGB/mix bg-rgb fg-rgb dulling-ratio)
        final-alpha     (* (RGB/alpha color) dulling-opacity)
        final-color     (RGB/withAlpha dulled-rgb final-alpha)]
    [final-color state]))

(defmethod mix :smudge
  [brush-spec params state data w h]
  (let [color           (:color brush-spec)                      ;; float[4] RGBA
        last-params     (:last-params state)
        smudge-radius   (float (or (util/param :smudge-radius params brush-spec) 10.0))
        smudge-strength (float (or (util/param :smudge-strength params brush-spec) 0.5))
        sample-x        (float (util/param :x last-params params))
        sample-y        (float (util/param :y last-params params))
        smudge-src      (Mix/sampleGaussian data (int w) (int h) sample-x sample-y smudge-radius)
        fg-rgb          (RGB/rgbaToRgb color)
        src-rgb         (RGB/rgbaToRgb smudge-src)
        mixed           (RGB/mix fg-rgb src-rgb smudge-strength)
        final-color     (RGB/withAlpha mixed (RGB/alpha color))]
    [final-color (assoc state :last-params params)]))