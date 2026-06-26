(ns top.kzre.krro.brush.vector.render
  "高效贝塞尔光栅化：基于距离场的扫描线算法，支持 2×2 子像素抗锯齿。"
  (:require [top.kzre.krro.brush.util :as util]
            [top.kzre.krro.brush.vector.fit :as fit]))

(defn- evaluate-bezier [[P0 P1 P2 P3] t]
  (let [u (- 1 t)]
    [(+ (* (nth P0 0) u u u) (* 3 (nth P1 0) u u t) (* 3 (nth P2 0) u t t) (* (nth P3 0) t t t))
     (+ (* (nth P0 1) u u u) (* 3 (nth P1 1) u u t) (* 3 (nth P2 1) u t t) (* (nth P3 1) t t t))]))

(defn- point-to-seg-dist [px py ax ay bx by]
  (let [dx (- bx ax) dy (- by ay)
        len2 (+ (* dx dx) (* dy dy))]
    (if (zero? len2)
      (util/distance [px py] [ax ay])
      (let [t (util/clamp 0.0 1.0 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) len2))
            proj-x (+ ax (* t dx))
            proj-y (+ ay (* t dy))]
        (util/distance [px py] [proj-x proj-y])))))

(def ^:private n-distance-samples 60)

(defn- min-dist-to-bezier [px py bezier]
  (let [samples (mapv #(evaluate-bezier bezier (/ % (dec n-distance-samples))) (range n-distance-samples))]
    (reduce (fn [best i]
              (let [[ax ay] (nth samples i)
                    [bx by] (nth samples (inc i))
                    d (point-to-seg-dist px py ax ay bx by)]
                (min best d)))
            Double/MAX_VALUE
            (range (dec n-distance-samples)))))

(defn- coverage [px py bezier half-w]
  (let [offsets [[-0.25 -0.25] [0.25 -0.25] [-0.25 0.25] [0.25 0.25]]
        hits (count (filter #(<= (min-dist-to-bezier (+ px (first %)) (+ py (second %)) bezier) half-w) offsets))]
    (/ hits 4.0)))

(defn- rasterize-segment [dab-size bezier width-fn]
  (let [data (double-array (* dab-size dab-size) 0.0)
        samples (mapv #(evaluate-bezier bezier (/ % (dec n-distance-samples))) (range n-distance-samples))
        xs (map first samples) ys (map second samples)
        min-x (apply min xs) max-x (apply max xs)
        min-y (apply min ys) max-y (apply max ys)
        max-width (width-fn 0.5)
        margin (int (Math/ceil (+ (/ max-width 2.0) 1.5)))
        lo-x (max 0 (int (- min-x margin)))
        lo-y (max 0 (int (- min-y margin)))
        hi-x (min (dec dab-size) (int (+ max-x margin)))
        hi-y (min (dec dab-size) (int (+ max-y margin)))
        half-w (/ max-width 2.0)]
    (doseq [py (range lo-y (inc hi-y))
            px (range lo-x (inc hi-x))]
      (let [d (min-dist-to-bezier px py bezier)]
        (when (<= d (+ half-w 1.5))
          (let [alpha (coverage px py bezier half-w)]
            (when (> alpha 0.0)
              (let [idx (+ px (* py dab-size))]
                (aset-double data idx (max (aget data idx) alpha))))))))
    data))

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
          (aset-double data idx (max (aget data idx) (aget seg-data idx))))))
    {:data data :width dab-size :height dab-size}))

(defn render-vector [vector-data width-fn dab-size]
  (render-vector-scanline vector-data width-fn dab-size))