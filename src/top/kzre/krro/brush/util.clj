(ns top.kzre.krro.brush.util
  (:import (top.kzre.colorutils MathUtils)))

(defn param
  "从多个 map 中按顺序查找关键字 kw，返回第一个非 nil 的值。
   若所有 map 都不包含该键，返回 nil。"
  [kw & maps]
  (some #(get % kw) maps))

(defn clamp01 [x]
  (MathUtils/clamp01 (float x)))

(defn v- [a b] (mapv - a b))

(defn dot [a b]
  (reduce + (map * a b)))

(defn length [v]
  (Math/sqrt (dot v v)))

(defn distance [a b]
  (length (v- a b)))

(defn lerp
  "线性插值。当 a, b 为数字时，返回 a + t * (b - a)。
   当 a, b 为向量时，逐元素插值。"
  [a b t]
  (if (number? a)
    (+ a (* t (- b a)))
    (mapv (fn [x y] (+ x (* t (- y x)))) a b)))

(defn ->string
  "将 keyword 或字符串转为字符串。nil 返回 nil。"
  [x]
  (when x (name x)))

(defn blend-mode-str
  "从 map 中提取 :blend-mode 并转为字符串，缺失返回默认字符串。"
  [m default]
  (->string (get m :blend-mode default)))