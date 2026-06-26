(ns top.kzre.krro.brush.vector.render
  "高效贝塞尔光栅化：基于距离场的扫描线算法，支持抗锯齿。"
  (:require [top.kzre.krro.brush.util :as util]
            [top.kzre.krro.brush.vector.fit :as fit]))

;; ── 贝塞尔求值 ────────────────────────────────────────
(defn- evaluate-bezier
  [[P0 P1 P2 P3] t]
  (let [u (- 1 t)]
    [(+ (* (nth P0 0) u u u) (* 3 (nth P1 0) u u t) (* 3 (nth P2 0) u t t) (* (nth P3 0) t t t))
     (+ (* (nth P0 1) u u u) (* 3 (nth P1 1) u u t) (* 3 (nth P2 1) u t t) (* (nth P3 1) t t t))]))

;; ── 点到线段距离 ──────────────────────────────────────
(defn- point-to-seg-dist [px py ax ay bx by]
  (let [dx (- bx ax) dy (- by ay)
        len2 (+ (* dx dx) (* dy dy))]
    (if (zero? len2)
      (util/distance [px py] [ax ay])
      (let [t (util/clamp 0.0 1.0 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) len2))
            proj-x (+ ax (* t dx))
            proj-y (+ ay (* t dy))]
        (util/distance [px py] [proj-x proj-y])))))

;; ── 点到贝塞尔最近距离（采样法）────────────────────────
(defn- min-dist-to-bezier [px py bezier n-samples]
  (let [samples (mapv #(evaluate-bezier bezier (/ % (dec n-samples))) (range n-samples))
        segs (mapv #(vector (nth samples %1) (nth samples (inc %1))) (range (dec n-samples)))]
    (apply min
           (for [[[ax ay] [bx by]] segs]
             (point-to-seg-dist px py ax ay bx by)))))

(defn create-smooth-width-fn
  "根据分段和基础宽度生成平滑线宽函数。
   segments: fit-curve 的输出列表（每个包含 :pressure）
   base-width: 基础线宽
   smooth-radius: 平滑窗口半径（段数），默认为 2"
  [segments base-width smooth-radius]
  (let [n (count segments)
        raw-widths (mapv (fn [seg] (* base-width (:pressure seg 1.0))) segments)
        smoothed (map-indexed
                   (fn [i _]
                     (let [start (max 0 (- i smooth-radius))
                           end (min n (+ i smooth-radius 1))
                           window (subvec raw-widths start end)]
                       (/ (apply + window) (count window))))
                   raw-widths)
        seg-ratio (/ 1.0 n)]
    (fn [t]
      (let [idx (min (dec n) (int (/ t seg-ratio)))
            next-idx (min (dec n) (inc idx))
            local-t (/ (- t (* idx seg-ratio)) seg-ratio)
            w1 (nth smoothed idx)
            w2 (nth smoothed next-idx)]
        (util/lerp w1 w2 (util/clamp 0.0 1.0 local-t))))))

;; ── 扫描线光栅化（单段） ──────────────────────────────
(defn- rasterize-segment [dab-size bezier width-fn]
  (let [data (double-array (* dab-size dab-size) 0.0)
        n-samples 50 ;; 贝塞尔采样点密度
        ;; 估算包围盒
        samples (mapv #(evaluate-bezier bezier (/ % (dec n-samples))) (range n-samples))
        xs (map first samples) ys (map second samples)
        min-x (apply min xs) max-x (apply max xs)
        min-y (apply min ys) max-y (apply max ys)
        margin 10 ;; 额外边界
        lo-x (max 0 (int (- min-x margin)))
        lo-y (max 0 (int (- min-y margin)))
        hi-x (min (dec dab-size) (int (+ max-x margin)))
        hi-y (min (dec dab-size) (int (+ max-y margin)))]
    (doseq [py (range lo-y (inc hi-y))
            px (range lo-x (inc hi-x))]
      (let [dist (min-dist-to-bezier px py bezier n-samples)
            ;; 计算当前点对应的曲线参数（近似：用采样点投影，但为简单使用位置比例）
            ;; 由于宽度沿曲线变化，我们取距离最小的采样点处的宽度
            ;; 简化：用整个段平均宽度
            mid-width (width-fn 0.5) ;; 后续可改为精确插值
            half-w (/ mid-width 2.0)
            alpha (if (<= dist half-w)
                    1.0
                    (if (<= dist (+ half-w 1.0))
                      (- 1.0 (- dist half-w))
                      0.0))]
        (when (> alpha 0.0)
          (let [idx (+ px (* py dab-size))
                old (aget data idx)]
            (aset-double data idx (max old alpha))))))
    data))

;; ── 主渲染函数（多段合成） ─────────────────────────────
(defn render-vector-scanline
  "使用扫描线算法将矢量笔触光栅化为灰度遮罩。
   vector-data :segments 为贝塞尔段列表
   width-fn 为 (fn [t] width) 函数，t 是全局参数 0..1
   dab-size  为光栅化画布尺寸（正方形边长）
   返回 {:data [double] :width dab-size :height dab-size}"
  [vector-data width-fn dab-size]
  (let [segments (:segments vector-data)
        data (double-array (* dab-size dab-size) 0.0)
        total-segs (count segments)]
    (doseq [i (range total-segs)]
      (let [seg (nth segments i)
            bezier [(:start seg) (:cp1 seg) (:cp2 seg) (:end seg)]
            ;; 为每个段生成独立的宽度函数（简化：使用段内平均压力计算宽度）
            seg-width-fn (fn [t] (width-fn (/ (+ i t) total-segs)))
            seg-data (rasterize-segment dab-size bezier seg-width-fn)]
        (dotimes [idx (* dab-size dab-size)]
          (aset-double data idx (max (aget data idx) (aget seg-data idx))))))
    {:data data :width dab-size :height dab-size}))

;; ── 保留旧版兼容函数 ──────────────────────────────────
(defn render-vector
  "旧版圆点叠加式光栅化，已废弃，请使用 render-vector-scanline"
  [vector-data width-fn dab-size]
  (render-vector-scanline vector-data width-fn dab-size))