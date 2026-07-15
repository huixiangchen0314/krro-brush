(ns top.kzre.krro.brush.core
  "笔刷引擎入口：重导出所有核心功能（重采样、笔触构建、渲染、混合、锥化、矢量）。"
  (:require
    [top.kzre.krro.brush.mix :as mix]
    [top.kzre.krro.brush.resample :as resample]
    [top.kzre.krro.brush.stroke :as stroke]
    [top.kzre.krro.brush.taper :as taper]
    [top.kzre.krro.brush.vector :as vector]))

;; 重采样
(def resample resample/resample)

;; 笔触构建与渲染
(def events->stroke stroke/events->stroke)
(def join-stroke stroke/join-stroke)
(def render-stroke-dirties! stroke/render-stroke-dirties!)
(def render-stroke! stroke/render-stroke!)

;; 动态混色（colored-brush, dulling, smudge 等）
(def mix mix/mix)

;; 锥化（支持分字段、定长/比例）
(def taper-stroke taper/taper-stroke)
(def taper-stroke-start taper/taper-stroke-start)
(def taper-stroke-end taper/taper-stroke-end)

;; 矢量笔触（预留）
(def generate-vector-stroke vector/generate-vector-stroke)