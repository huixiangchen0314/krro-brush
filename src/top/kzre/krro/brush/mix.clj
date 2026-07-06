(ns top.kzre.krro.brush.mix
  "混合模式模块：根据 mix-mode 动态计算前景色，并维护状态。
   不再依赖画布协议，直接操作 double 数组。"
  (:require [top.kzre.krro.brush.util :as util])
  (:import (top.kzre.colorutils.color RGB)
           (top.kzre.krro.brush Mix)))

;; ── 混合多方法 ─────────────────────────────────────
(defmulti mix
          "根据 brush-spec 中的 :mix-mode 计算最终前景色，并返回新的状态。
           参数：brush-spec, params, state, data, w, h。
           返回向量 [fg-rgba new-state]。"
          (fn [brush-spec _params _state _data _w _h] (:mix-mode brush-spec)))

(defmethod mix :default
  [{:keys [color]} _params state _data _w _h]
  [color state])

(defmethod mix :colored-brush
  [brush-spec params state data w h]
  (let [color           (:color brush-spec)                      ;; double[4] RGBA
        blend-ratio     (util/param :blend-ratio params brush-spec 0.5)
        carry-decay     (util/param :carry-decay params brush-spec 0.5)
        decay-exponent  (util/param :decay-exponent params brush-spec 1.0)
        min-carry       (util/param :min-carry params brush-spec 0.0)
        carry           (or (:carry-color state)                 ;; state 里存 double[3] RGB
                            (RGB/rgbaToRgb color))
        ;; 单点背景采样（radius=0）
        bg-rgba         (Mix/sampleGaussian data (int w) (int h)
                                            (double (:x params)) (double (:y params)) 0.0)
        fg-rgb          (RGB/rgbaToRgb color)
        bg-rgb          (RGB/rgbaToRgb bg-rgba)
        blended         (RGB/mix fg-rgb bg-rgb blend-ratio)      ;; double[3]
        effective-decay (Math/pow carry-decay decay-exponent)
        clamped-decay   (max min-carry effective-decay)
        final-rgb       (RGB/mix carry blended clamped-decay)    ;; double[3]
        final-rgba      (RGB/withAlpha final-rgb (RGB/alpha color))]
    [final-rgba (assoc state :carry-color final-rgb)]))           ;; 状态存 RGB

(defmethod mix :dulling
  [brush-spec params state data w h]
  (let [color           (:color brush-spec)                         ;; double[4] RGBA
        dulling-ratio   (util/param :dulling-ratio params brush-spec 0.5)
        dulling-opacity (util/param :dulling-opacity params brush-spec 1.0)
        ;; 获取背景单点颜色（radius=0）
        bg-rgba         (Mix/sampleGaussian data (int w) (int h)
                                            (double (:x params)) (double (:y params)) 0.0)
        fg-rgb          (RGB/rgbaToRgb color)
        bg-rgb          (RGB/rgbaToRgb bg-rgba)
        ;; 背景向前景混合 dulling-ratio，即 lerp(bg, fg, dulling-ratio)
        dulled-rgb      (RGB/mix bg-rgb fg-rgb dulling-ratio)
        final-alpha     (* (RGB/alpha color) dulling-opacity)
        final-color     (RGB/withAlpha dulled-rgb final-alpha)]
    [final-color state]))

(defmethod mix :smudge
  [brush-spec params state data w h]
  (let [color           (:color brush-spec)                         ;; double[4] RGBA
        last-params     (:last-params state)
        smudge-radius   (or (util/param :smudge-radius params brush-spec) 10)
        smudge-strength (or (util/param :smudge-strength params brush-spec) 0.5)
        sample-x        (util/param :x last-params params)
        sample-y        (util/param :y last-params params)
        smudge-src      (Mix/sampleGaussian data w h sample-x sample-y smudge-radius)
        fg-rgb          (RGB/rgbaToRgb color)
        src-rgb         (RGB/rgbaToRgb smudge-src)
        mixed         (RGB/mix fg-rgb src-rgb smudge-strength)
        final-color     (RGB/withAlpha mixed (RGB/alpha color))]
    [final-color (assoc state :last-params params)]))