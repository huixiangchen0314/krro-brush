(ns top.kzre.krro.brush.support.linear
  "小型线性代数工具：2x2 / 3x3 矩阵求逆、乘法及线性方程组求解。")

(defn matrix-mult
  "矩阵乘向量。m 为 2x2 或 3x3 矩阵（嵌套向量），v 为向量。"
  [m v]
  (mapv #(apply + (map * % v)) m))

(defn matrix-2x2-inv
  "计算 2x2 矩阵的逆。返回 nil 如果不可逆。"
  [[[a b] [c d]]]
  (let [det (- (* a d) (* b c))]
    (when (not (zero? det))
      (let [inv-det (/ 1.0 det)]
        [[(* d inv-det) (* (- b) inv-det)]
         [(* (- c) inv-det) (* a inv-det)]]))))

(defn matrix-3x3-inv
  "计算 3x3 矩阵的逆。返回 nil 如果不可逆。"
  [[[a b c] [d e f] [g h i]]]
  (let [det (+ (* a (- (* e i) (* f h)))
               (* b (- (* f g) (* d i)))
               (* c (- (* d h) (* e g))))]
    (when (not (zero? det))
      (let [inv-det (/ 1.0 det)]
        [[(* inv-det (- (* e i) (* f h)))
          (* inv-det (- (* c h) (* b i)))
          (* inv-det (- (* b f) (* c e)))]
         [(* inv-det (- (* f g) (* d i)))
          (* inv-det (- (* a i) (* c g)))
          (* inv-det (- (* c d) (* a f)))]
         [(* inv-det (- (* d h) (* e g)))
          (* inv-det (- (* b g) (* a h)))
          (* inv-det (- (* a e) (* b d)))]]))))

(defn solve-2x2
  "求解线性方程组 Ax = b，其中 A 为 2x2 矩阵，b 为长度 2 向量。
   返回解 x，如果无解返回 nil。"
  [A b]
  (when-let [inv (matrix-2x2-inv A)]
    (matrix-mult inv b)))

(defn solve-3x3
  "求解线性方程组 Ax = b，其中 A 为 3x3 矩阵，b 为长度 3 向量。
   返回解 x，如果无解返回 nil。"
  [A b]
  (when-let [inv (matrix-3x3-inv A)]
    (matrix-mult inv b)))