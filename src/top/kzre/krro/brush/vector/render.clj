(ns top.kzre.krro.brush.vector.render
  "高效贝塞尔光栅化：基于距离场的扫描线算法，支持 2×2 子像素抗锯齿。"
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

;; ── 点到贝塞尔最近距离（增加采样密度）───────────────
(defn- min-dist-to-bezier [px py bezier n-samples]
  (let [samples (mapv #(evaluate-bezier bezier (/ % (dec n-samples))) (range n-samples))
        segs (mapv #(vector (nth samples %1) (nth samples (inc %1))) (range (dec n-samples)))]
    (apply min
           (for [[[ax ay] [bx by]] segs]
             (point-to-seg-dist px py ax ay bx by)))))

;; ── 抗锯齿覆盖：2×2 子像素 ─────────────────────────────
(defn- coverage [px py bezier half-w n-samples]
  (let [offsets [[-0.25 -0.25] [ 0.25 -0.25] [-0.25  0.25] [ 0.25  0.25]]
        hits (reduce
               (fn [acc [ox oy]]
                 (let [sx (+ px ox)
                       sy (+ py oy)
                       d (min-dist-to-bezier sx sy bezier n-samples)]
                   (if (<= d half-w) (inc acc) acc)))
               0
               offsets)]
    (/ hits 4.0)))

;; ── 扫描线光栅化（单段，带抗锯齿）─────────────────────
(defn- rasterize-segment [dab-size bezier width-fn n-samples]
  (let [data (double-array (* dab-size dab-size) 0.0)
        ;; 估算包围盒
        samples (mapv #(evaluate-bezier bezier (/ % (dec n-samples))) (range n-samples))
        xs (map first samples) ys (map second samples)
        min-x (apply min xs) max-x (apply max xs)
        min-y (apply min ys) max-y (apply max ys)
        margin 10
        lo-x (max 0 (int (- min-x margin)))
        lo-y (max 0 (int (- min-y margin)))
        hi-x (min (dec dab-size) (int (+ max-x margin)))
        hi-y (min (dec dab-size) (int (+ max-y margin)))
        ;; 段中点用于粗略宽度（后续可改为插值）
        mid-width (width-fn 0.5)
        half-w (/ mid-width 2.0)]
    (doseq [py (range lo-y (inc hi-y))
            px (range lo-x (inc hi-x))]
      ;; 快速排除：计算像素中心的距离，若大于半宽+子像素对角线的一半则跳过
      (let [center-dist (min-dist-to-bezier px py bezier n-samples)
            diag-half (/ (Math/sqrt 2) 2)] ;; 子像素对角的一半
        (when (<= center-dist (+ half-w diag-half))
          (let [alpha (coverage px py bezier half-w n-samples)]
            (when (> alpha 0.0)
              (let [idx (+ px (* py dab-size))
                    old (aget data idx)]
                (aset-double data idx (max old alpha))))))))
    data))

;; ── 主渲染函数（多段合成） ─────────────────────────────
(defn render-vector-scanline
  "使用扫描线算法将矢量笔触光栅化为灰度遮罩，带 2×2 抗锯齿。
   vector-data :segments 为贝塞尔段列表
   width-fn 为 (fn [t] width) 函数，t 是全局参数 0..1
   dab-size  为光栅化画布尺寸（正方形边长）"
  [vector-data width-fn dab-size]
  (let [segments (:segments vector-data)
        data (double-array (* dab-size dab-size) 0.0)
        total-segs (count segments)
        n-samples 100]  ;; 贝塞尔采样精度
    (doseq [i (range total-segs)]
      (let [seg (nth segments i)
            bezier [(:start seg) (:cp1 seg) (:cp2 seg) (:end seg)]
            seg-width-fn (fn [t] (width-fn (/ (+ i t) total-segs)))
            seg-data (rasterize-segment dab-size bezier seg-width-fn n-samples)]
        (dotimes [idx (* dab-size dab-size)]
          (aset-double data idx (max (aget data idx) (aget seg-data idx))))))
    {:data data :width dab-size :height dab-size}))

;; ── 保留旧版兼容 ──────────────────────────────────────
(defn render-vector [vector-data width-fn dab-size]
  (render-vector-scanline vector-data width-fn dab-size))