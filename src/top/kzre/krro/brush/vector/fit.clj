(ns top.kzre.krro.brush.vector.fit
  "三次贝塞尔曲线拟合：使用自适应误差分段 (FitCurve) 算法，
   保证段间 G1 连续性（切线方向一致）。"
  (:require [top.kzre.krro.brush.util :as util]
            [top.kzre.krro.brush.support.linear :as linear]))

;; ── 辅助函数 ──────────────────────────────────────────
(defn- pos [pt] [(:x pt) (:y pt)])

(defn- chord-length-parameterize
  "根据弦长对点序列进行参数化，返回 t ∈ [0,1] 的序列。"
  [points]
  (let [n (count points)]
    (if (<= n 2)
      (mapv #(/ % (dec n)) (range n))
      (let [dists (mapv (fn [a b] (util/distance (pos a) (pos b)))
                        points (rest points))
            total (apply + dists)
            cum   (reductions + dists)]
        (mapv #(/ % total) (cons 0 cum))))))

;; ── 单段贝塞尔最小二乘拟合（已实现） ─────────────────
(defn- fit-single-bezier
  "用最小二乘法将一组点拟合为一条三次贝塞尔曲线。
   返回 [P0 P1 P2 P3] 四个控制点。"
  [pts ts]
  (let [n (count pts)
        P0 (pos (first pts))
        P3 (pos (last pts))
        a-coeffs (mapv (fn [t] (* 3 (Math/pow (- 1 t) 2) t)) ts)
        b-coeffs (mapv (fn [t] (* 3 (- 1 t) (Math/pow t 2))) ts)
        Qs (mapv (fn [pt a b t]
                   (let [Q (mapv - (pos pt)
                                 (mapv #(* (Math/pow (- 1 t) 3) %) P0)
                                 (mapv #(* (Math/pow t 3) %) P3))]
                     Q))
                 pts a-coeffs b-coeffs ts)
        sum-aa (apply + (map #(* % %) a-coeffs))
        sum-ab (apply + (map * a-coeffs b-coeffs))
        sum-bb (apply + (map #(* % %) b-coeffs))
        ATA [[sum-aa sum-ab] [sum-ab sum-bb]]
        bx [(apply + (mapv (fn [a q] (* a (first q))) a-coeffs Qs))
            (apply + (mapv (fn [b q] (* b (first q))) b-coeffs Qs))]
        by [(apply + (mapv (fn [a q] (* a (second q))) a-coeffs Qs))
            (apply + (mapv (fn [b q] (* b (second q))) b-coeffs Qs))]]
    (if-let [sol-x (linear/solve-2x2 ATA bx)]
      (if-let [sol-y (linear/solve-2x2 ATA by)]
        [P0 [(first sol-x) (first sol-y)] [(second sol-x) (second sol-y)] P3]
        ;; 退化为简单三等分
        [P0 (mapv + P0 (mapv #(/ % 3) (mapv - P3 P0)))
         (mapv - P3 (mapv #(/ % 3) (mapv - P3 P0))) P3])
      [P0 (mapv + P0 (mapv #(/ % 3) (mapv - P3 P0)))
       (mapv - P3 (mapv #(/ % 3) (mapv - P3 P0))) P3])))

;; ── 贝塞尔曲线求值 ────────────────────────────────────
(defn- evaluate-bezier
  "计算三次贝塞尔曲线上参数 t 处的点。"
  [[P0 P1 P2 P3] t]
  (let [u (- 1 t)]
    [(+ (* (nth P0 0) u u u) (* 3 (nth P1 0) u u t) (* 3 (nth P2 0) u t t) (* (nth P3 0) t t t))
     (+ (* (nth P0 1) u u u) (* 3 (nth P1 1) u u t) (* 3 (nth P2 1) u t t) (* (nth P3 1) t t t))]))

;; ── 拟合误差计算 ──────────────────────────────────────
(defn- compute-error
  "计算原始点与贝塞尔曲线之间的最大距离。"
  [pts bezier ts]
  (apply max
         (map-indexed (fn [i pt]
                        (let [curve-pt (evaluate-bezier bezier (nth ts i))]
                          (util/distance (pos pt) curve-pt)))
                      pts)))

;; ── 自适应分段拟合 (FitCurve) ─────────────────────────
(defn fit-curve
  "将点序列自适应分段拟合为贝塞尔曲线列表，保证误差不超过 max-error。
   返回的每段为 {:start P0, :cp1 P1, :cp2 P2, :end P3, :pressure avg-pressure}。"
  ([points] (fit-curve points 4.0))  ;; 默认最大误差 4 像素
  ([points max-error]
   (if (< (count points) 2)
     []
     (let [ts (chord-length-parameterize points)
           bezier (fit-single-bezier points ts)
           err (compute-error points bezier ts)]
       (if (or (<= err max-error) (<= (count points) 3))
         ;; 误差可接受或点数太少，直接返回一段
         (let [avg-pressure (/ (apply + (map :pressure points)) (count points))]
           [{:start (first bezier)
             :cp1 (second bezier)
             :cp2 (nth bezier 2)
             :end (nth bezier 3)
             :pressure avg-pressure}])
         ;; 分割并递归拟合
         (let [split-idx (atom 0)
               _ (dotimes [i (count points)]
                   (let [curve-pt (evaluate-bezier bezier (nth ts i))
                         dist (util/distance (pos (nth points i)) curve-pt)]
                     (when (> dist (compute-error [(nth points @split-idx)] bezier [(nth ts @split-idx)]))
                       (reset! split-idx i))))
               left-pts (subvec (vec points) 0 (inc @split-idx))
               right-pts (subvec (vec points) @split-idx)]
           (concat (fit-curve left-pts max-error)
                   (fit-curve right-pts max-error))))))))