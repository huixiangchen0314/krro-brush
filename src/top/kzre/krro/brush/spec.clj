(ns top.kzre.krro.brush.spec
  "笔刷数据规格定义，用于验证和文档化笔刷数据结构。
   使用 clojure.spec.alpha 描述笔刷 map 的各个字段。"
  (:require [clojure.spec.alpha :as s]))

;; ── 基础类型 ────────────────────────────────────────────
(s/def ::id keyword?)
(s/def ::name string?)
(s/def ::version string?)

;; ── 笔尖 (Dab) 规格 ────────────────────────────────────
(s/def ::dab-type #{:circle :image :custom})
(s/def ::radius number?)
(s/def ::hardness (s/and number? #(<= 0.0 % 1.0)))
(s/def ::aspect-ratio number?)
(s/def ::angle number?)

(s/def ::dab
  (s/keys :req-un [::dab-type ::radius ::hardness ::aspect-ratio ::angle]
          :opt-un [::texture-source]))

;; ── 纹理 (Texture) 规格 ────────────────────────────────
(s/def ::texture-source some? )  ;; 资源引用，可以是字符串或 nil
(s/def ::texture-scale number?)
(s/def ::texture-mode #{:multiply :overlay :normal :screen})

(s/def ::texture
  (s/keys :opt-un [::texture-source ::texture-scale ::texture-mode]))

;; ── 颜色行为 (Color) 规格 ──────────────────────────────
(s/def ::color-source #{:foreground :gradient :track})
(s/def ::blend-model #{:basic :colored-brush :kubelka-munk :smear})  ;; 修改这里
(s/def ::carry-decay (s/and number? #(<= 0.0 % 1.0)))
(s/def ::pigment-lib (s/nilable map?))

(s/def ::color
  (s/keys :req-un [::color-source ::blend-model]
          :opt-un [::carry-decay ::pigment-lib]))

;; ── 动力学映射 (Dynamics) 规格 ─────────────────────────
(s/def ::curve #{:linear :sigmoid :quadratic :cubic})
(s/def ::min number?)
(s/def ::max number?)

;; 单个映射条目：{:curve :linear :min 0.2 :max 1.0}
(s/def ::mapping
  (s/keys :req-un [::curve ::min ::max]))

;; 动力学规格第一层是传感器名称（keyword），值为一个 map，其键是目标参数，值是指定映射
(s/def ::sensor-dynamics
  (s/map-of keyword? (s/map-of keyword? ::mapping)))

(s/def ::dynamics
  (s/map-of keyword? ::sensor-dynamics))

;; ── 笔触控制 (Stroke) 规格 ─────────────────────────────
(s/def ::spacing number?)
(s/def ::stabilizer #{:none :gaussian :kalman})
(s/def ::smoothing number?)

(s/def ::stroke
  (s/keys :req-un [::spacing ::stabilizer ::smoothing]))

;; ── 后处理 (Post-processing) 规格 ──────────────────────
(s/def ::watercolor-edge boolean?)
(s/def ::edge-intensity number?)
(s/def ::paper-source some?)
(s/def ::paper-strength number?)

(s/def ::watercolor
  (s/keys :req-un [::watercolor-edge ::edge-intensity]))
(s/def ::paper
  (s/keys :req-un [::paper-source ::paper-strength]))

(s/def ::post
  (s/keys :opt-un [::watercolor ::paper]))

;; ── 完整笔刷定义 ───────────────────────────────────────
(s/def ::brush
  (s/keys :req-un [::id ::name ::version ::dab ::color ::dynamics ::stroke]
          :opt-un [::texture ::post]))

;; 输入事件字段约定（非强制，仅文档）
(s/def ::event-x number?)
(s/def ::event-y number?)
(s/def ::event-pressure (s/and number? #(<= 0.0 % 1.0)))
(s/def ::event-velocity (s/and number? #(<= 0.0 % 1.0)))
(s/def ::event-timestamp number?)
;; 注意：不强制要求输入事件包含这些字段，核心代码会提供默认值