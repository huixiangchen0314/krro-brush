(ns top.kzre.krro.brush.util
  "内部数学与数组工具，纯函数，无副作用。")

;; ── 标量运算 ─────────────────────────────────────────
(defn clamp
  "将 x 限制在 [lo, hi] 闭区间内。"
  [lo hi x]
  (max lo (min hi x)))

(defn deg->rad
  "角度 → 弧度。"
  [d]
  (* d (/ Math/PI 180.0)))

(defn rad->deg
  "弧度 → 角度。"
  [r]
  (* r (/ 180.0 Math/PI)))

(defn lerp
  "线性插值。t ∈ [0,1] 时返回 a→b 的插值，超出区间外推。"
  [a b t]
  (+ a (* t (- b a))))

(defn sigmoid
  "标准 Logistic 函数，将 x 映射到 (0,1)。"
  [x]
  (/ 1.0 (+ 1.0 (Math/exp (- x)))))

;; ── 向量运算（所有向量均为 Clojure 向量或数字集合）───
(defn v+ [a b] (mapv + a b))
(defn v- [a b] (mapv - a b))
(defn v* [a s] (mapv #(* % s) a))

(defn dot [a b]
  (reduce + (map * a b)))

(defn length [v]
  (Math/sqrt (dot v v)))

(defn distance [a b]
  (length (v- a b)))

(defn normalize [v]
  (let [len (length v)]
    (if (zero? len) v (v* v (/ 1.0 len)))))

(defn avg-point
  "多个向量的分量平均。"
  [pts]
  (let [n (count pts)]
    (if (zero? n) [0 0]
                  (v* (reduce v+ pts) (/ 1.0 n)))))

;; ── 数组操作（针对 double 数组）─────────────────────
(defn double-array-2d
  "创建一个宽度 w、高度 h 的一维 double 数组（行主序），初始值为 0.0。"
  [w h]
  (double-array (* w h) 0.0))

(defn array-2d-get
  "从行主序一维数组中获取 (x,y) 处的值。"
  [arr w x y]
  (aget arr (+ x (* y w))))

(defn array-2d-set
  "在行主序一维数组中设置 (x,y) 处的值。"
  [arr w x y val]
  (aset arr (+ x (* y w)) val)
  arr)