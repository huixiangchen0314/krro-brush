(ns top.kzre.krro.brush.vector.fit
  "三次贝塞尔曲线拟合：使用自适应误差分段 (FitCurve) 算法。
   处理退化情况（点不足、共线），保证数值稳定性。"
  (:require [top.kzre.krro.brush.util :as util]
            [top.kzre.krro.brush.support.linear :as linear]))

(defn- pos [pt] [(:x pt) (:y pt)])

(defn- chord-length-parameterize
  [points]
  (let [n (count points)]
    (if (<= n 2)
      (mapv #(/ % (dec n)) (range n))
      (let [dists (mapv (fn [a b] (util/distance (pos a) (pos b))) points (rest points))
            total (apply + dists)
            cum   (reductions + dists)]
        (mapv #(/ % total) (cons 0 cum))))))

;; 检查点集是否近似共线（叉积和接近零）
(defn- collinear?
  [points]
  (let [p0 (pos (first points))
        pn (pos (last points))
        dir (mapv - pn p0)
        dir-len (util/length dir)]
    (if (< dir-len 1e-10) true ;; 所有点重合
                          (let [perp [(- (second dir)) (first dir)]
                                dists (map #(Math/abs (util/dot (mapv - (pos %) p0) perp)) (rest (butlast points)))]
                            (every? #(< % 1e-6) dists)))))

(defn- fit-single-bezier
  [pts ts]
  (let [n (count pts)
        P0 (pos (first pts))
        P3 (pos (last pts))
        a-coeffs (mapv (fn [t] (* 3 (Math/pow (- 1 t) 2) t)) ts)
        b-coeffs (mapv (fn [t] (* 3 (- 1 t) (Math/pow t 2))) ts)
        Qs (mapv (fn [pt a b t]
                   (mapv - (pos pt)
                         (mapv #(* (Math/pow (- 1 t) 3) %) P0)
                         (mapv #(* (Math/pow t 3) %) P3)))
                 pts a-coeffs b-coeffs ts)
        sum-aa (apply + (map * a-coeffs a-coeffs))
        sum-ab (apply + (map * a-coeffs b-coeffs))
        sum-bb (apply + (map * b-coeffs b-coeffs))
        ATA [[sum-aa sum-ab] [sum-ab sum-bb]]
        bx [(apply + (mapv (fn [a q] (* a (first q))) a-coeffs Qs))
            (apply + (mapv (fn [b q] (* b (first q))) b-coeffs Qs))]
        by [(apply + (mapv (fn [a q] (* a (second q))) a-coeffs Qs))
            (apply + (mapv (fn [b q] (* b (second q))) b-coeffs Qs))]]
    (if-let [sol-x (linear/solve-2x2 ATA bx)]
      (if-let [sol-y (linear/solve-2x2 ATA by)]
        [P0 [(first sol-x) (first sol-y)] [(second sol-x) (second sol-y)] P3]
        ;; 退化为三等分
        [P0 (mapv + P0 (mapv #(/ % 3) (mapv - P3 P0)))
         (mapv - P3 (mapv #(/ % 3) (mapv - P3 P0))) P3])
      ;; 退化为三等分
      [P0 (mapv + P0 (mapv #(/ % 3) (mapv - P3 P0)))
       (mapv - P3 (mapv #(/ % 3) (mapv - P3 P0))) P3])))

(defn evaluate-bezier
  [[P0 P1 P2 P3] t]
  (let [u (- 1 t)]
    [(+ (* (nth P0 0) u u u) (* 3 (nth P1 0) u u t) (* 3 (nth P2 0) u t t) (* (nth P3 0) t t t))
     (+ (* (nth P0 1) u u u) (* 3 (nth P1 1) u u t) (* 3 (nth P2 1) u t t) (* (nth P3 1) t t t))]))

(defn- compute-error
  [pts bezier ts]
  (apply max
         (map-indexed (fn [i pt]
                        (let [curve-pt (evaluate-bezier bezier (nth ts i))]
                          (util/distance (pos pt) curve-pt)))
                      pts)))

(defn- fit-curve*
  [points max-error]
  (if (< (count points) 2)
    []
    (if (collinear? points)
      ;; 共线：直接生成直线贝塞尔
      (let [P0 (pos (first points))
            P3 (pos (last points))
            avg-pressure (/ (apply + (map :pressure points)) (count points))]
        [{:start P0
          :cp1 (mapv + P0 (mapv #(/ % 3) (mapv - P3 P0)))
          :cp2 (mapv - P3 (mapv #(/ % 3) (mapv - P3 P0)))
          :end P3
          :pressure avg-pressure}])
      ;; 正常拟合
      (let [ts (chord-length-parameterize points)
            bezier (fit-single-bezier points ts)
            err (compute-error points bezier ts)]
        (if (or (<= err max-error) (<= (count points) 3))
          (let [avg-pressure (/ (apply + (map :pressure points)) (count points))]
            [{:start (first bezier)
              :cp1 (second bezier)
              :cp2 (nth bezier 2)
              :end (nth bezier 3)
              :pressure avg-pressure}])
          (let [split-idx (atom 0)
                max-dist (atom 0.0)]
            (doseq [i (range (count points))]
              (let [curve-pt (evaluate-bezier bezier (nth ts i))
                    dist (util/distance (pos (nth points i)) curve-pt)]
                (when (> dist @max-dist)
                  (reset! max-dist dist)
                  (reset! split-idx i))))
            (let [left-pts (subvec (vec points) 0 (inc @split-idx))
                  right-pts (subvec (vec points) @split-idx)]
              (concat (fit-curve* left-pts max-error)
                      (fit-curve* right-pts max-error)))))))))

(defn fit-curve
  "将点序列自适应分段拟合为贝塞尔曲线列表，返回向量。"
  ([points] (vec (fit-curve* points 4.0)))
  ([points max-error] (vec (fit-curve* points max-error))))