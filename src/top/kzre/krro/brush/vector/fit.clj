(ns top.kzre.krro.brush.vector.fit
  "三次贝塞尔拟合：使用最小二乘法精确求解控制点。
   依赖线性代数工具 support.linear。"
  (:require [top.kzre.krro.brush.util :as util]
            [top.kzre.krro.brush.support.linear :as linear]))

(defn- pos [pt] [(:x pt) (:y pt)])

(defn- chord-length-parameterize
  "根据弦长对点序列进行参数化，返回参数 t ∈ [0,1] 的序列。"
  [points]
  (let [n (count points)]
    (if (<= n 2)
      (mapv #(/ % (dec n)) (range n))
      (let [dists (mapv (fn [a b] (util/distance (pos a) (pos b)))
                        points (rest points))
            total (apply + dists)
            cum   (reductions + dists)]
        (mapv #(/ % total) (cons 0 cum))))))

(defn- fit-single-bezier
  "用最小二乘法将一组点拟合为一条三次贝塞尔曲线。
   返回 [P0 P1 P2 P3] 四个控制点（向量 [x y]）。"
  [pts ts]
  (let [n (count pts)
        P0 (pos (first pts))
        P3 (pos (last pts))
        ;; 构造最小二乘矩阵：对每个点计算系数
        a-coeffs (mapv (fn [t] (* 3 (Math/pow (- 1 t) 2) t)) ts)
        b-coeffs (mapv (fn [t] (* 3 (- 1 t) (Math/pow t 2))) ts)
        ;; 计算 Q = B(t) - (1-t)^3 P0 - t^3 P3
        Qs (mapv (fn [pt a b t]
                   (let [Q (mapv - (pos pt)
                                 (mapv #(* (Math/pow (- 1 t) 3) %) P0)
                                 (mapv #(* (Math/pow t 3) %) P3))]
                     Q))
                 pts a-coeffs b-coeffs ts)
        ;; 正规方程 A^T A 和 A^T b
        sum-aa (apply + (map #(* % %) a-coeffs))
        sum-ab (apply + (map * a-coeffs b-coeffs))
        sum-bb (apply + (map #(* % %) b-coeffs))
        ATA [[sum-aa sum-ab] [sum-ab sum-bb]]
        ;; 分别对 x, y 坐标求解
        bx [(apply + (mapv (fn [a q] (* a (first q))) a-coeffs Qs))
            (apply + (mapv (fn [b q] (* b (first q))) b-coeffs Qs))]
        by [(apply + (mapv (fn [a q] (* a (second q))) a-coeffs Qs))
            (apply + (mapv (fn [b q] (* b (second q))) b-coeffs Qs))]]
    (if-let [sol-x (linear/solve-2x2 ATA bx)]
      (if-let [sol-y (linear/solve-2x2 ATA by)]
        ;; 解出 P1, P2
        [P0
         [(first sol-x) (first sol-y)]
         [(second sol-x) (second sol-y)]
         P3]
        ;; 退化为简单三等分
        [P0
         (mapv + P0 (mapv #(/ % 3) (mapv - P3 P0)))
         (mapv - P3 (mapv #(/ % 3) (mapv - P3 P0)))
         P3])
      ;; 退化为简单三等分
      [P0
       (mapv + P0 (mapv #(/ % 3) (mapv - P3 P0)))
       (mapv - P3 (mapv #(/ % 3) (mapv - P3 P0)))
       P3])))

(defn fit-curve
  "将点序列分段拟合为贝塞尔曲线列表。
   每个曲线为 {:start [x y], :cp1 [x y], :cp2 [x y], :end [x y]}
   同时附带宽度信息（基于压力）。"
  [points]
  (if (empty? points)
    []
    (let [ts (chord-length-parameterize points)
          ;; 分段：每4个点一组，重叠3个点以保证连续
          segments (partition-all 4 3 points)]
      (mapv (fn [seg]
              (let [seg-pts (vec seg)
                    seg-ts  (if (> (count seg-pts) 1)
                              (chord-length-parameterize seg-pts)
                              [0 1])]
                (let [[P0 P1 P2 P3] (fit-single-bezier seg-pts seg-ts)
                      avg-pressure (if (seq seg-pts)
                                     (/ (apply + (map :pressure seg-pts)) (count seg-pts))
                                     1.0)]
                  {:start P0 :cp1 P1 :cp2 P2 :end P3
                   :pressure avg-pressure})))
            segments))))