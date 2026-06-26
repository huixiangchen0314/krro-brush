(ns top.kzre.krro.brush.vector.render
  "高效贝塞尔光栅化：基于距离场的扫描线算法，使用连续距离抗锯齿。
   优化了距离计算（复用采样点）并移除了多重子像素采样，性能大幅提升。"
  (:require
   [top.kzre.krro.brush.protocol :as p]
   [top.kzre.krro.brush.util :as util]))

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

;; ── 点到贝塞尔最近距离（接收预计算的采样点）─────────────
(def ^:private n-distance-samples 40)

(defn- min-dist-to-bezier [px py samples]
  (reduce (fn [best i]
            (let [[ax ay] (nth samples i)
                  [bx by] (nth samples (inc i))
                  d (point-to-seg-dist px py ax ay bx by)]
              (min best d)))
          Double/MAX_VALUE
          (range (dec (count samples)))))

;; ── 连续距离抗锯齿 ─────────────────────────────────────
(defn- smooth-alpha [dist half-w]
  (let [inner (- half-w 0.5)
        outer (+ half-w 0.5)
        t (/ (- dist inner) (- outer inner))]
    (util/clamp 0.0 1.0 (- 1.0 t))))

;; ── 扫描线光栅化（单段，带抗锯齿）─────────────────────
(defn- rasterize-segment [dab-size bezier width-fn]
  (let [data (double-array (* dab-size dab-size) 0.0)
        ;; 预计算贝塞尔采样点，复用给所有像素
        samples (mapv #(evaluate-bezier bezier (/ % (dec n-distance-samples))) (range n-distance-samples))
        xs (map first samples) ys (map second samples)
        min-x (apply min xs) max-x (apply max xs)
        min-y (apply min ys) max-y (apply max ys)
        max-width (width-fn 0.5)
        half-w (max 0.5 (/ max-width 2.0))
        margin (int (Math/ceil (+ half-w 1.0)))
        lo-x (max 0 (int (- min-x margin)))
        lo-y (max 0 (int (- min-y margin)))
        hi-x (min (dec dab-size) (int (+ max-x margin)))
        hi-y (min (dec dab-size) (int (+ max-y margin)))]
    (doseq [py (range lo-y (inc hi-y))
            px (range lo-x (inc hi-x))]
      (let [d (min-dist-to-bezier px py samples)
            alpha (smooth-alpha d half-w)]
        (when (> alpha 0.0)
          (let [idx (+ px (* py dab-size))]
            (aset-double data idx (util/clamp 0.0 1.0 (max (aget data idx) alpha)))))))
    data))

;; ── 线宽平滑插值 ──────────────────────────────────────
(defn create-smooth-width-fn
  [segments base-width smooth-radius]
  (let [n (count segments)
        raw (mapv (fn [seg] (* base-width (:pressure seg 1.0))) segments)
        smoothed (map-indexed (fn [i _]
                                (let [start (max 0 (- i smooth-radius))
                                      end (min n (+ i smooth-radius 1))
                                      window (subvec raw start end)]
                                  (/ (apply + window) (count window))))
                              raw)
        seg-ratio (/ 1.0 n)]
    (fn [t]
      (let [idx (min (dec n) (int (/ t seg-ratio)))
            next-idx (min (dec n) (inc idx))
            local-t (util/clamp 0.0 1.0 (/ (- t (* idx seg-ratio)) seg-ratio))
            w1 (nth smoothed idx)
            w2 (nth smoothed next-idx)]
        (util/lerp w1 w2 local-t)))))

;; ── 主渲染函数（多段合成） ─────────────────────────────
(defn render-vector-scanline
  [vector-data width-fn dab-size]
  (let [segments (:segments vector-data)
        data (double-array (* dab-size dab-size) 0.0)
        total (count segments)]
    (doseq [i (range total)]
      (let [seg (nth segments i)
            bezier [(:start seg) (:cp1 seg) (:cp2 seg) (:end seg)]
            seg-w (fn [t] (width-fn (/ (+ i t) total)))
            seg-data (rasterize-segment dab-size bezier seg-w)]
        (dotimes [idx (* dab-size dab-size)]
          (aset-double data idx (util/clamp 0.0 1.0 (max (aget data idx) (aget seg-data idx)))))))
    {:data data :width dab-size :height dab-size}))

(defn render-vector [vector-data width-fn dab-size]
  (render-vector-scanline vector-data width-fn dab-size))

(defrecord DefaultVectorRasterizer []
  p/IVectorRasterizer
  (rasterize-vector [_ segments width-fn dab-size]
    (render-vector-scanline {:segments segments} width-fn dab-size)))

(def default-vector-rasterizer (->DefaultVectorRasterizer))
