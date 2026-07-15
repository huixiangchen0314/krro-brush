(ns top.kzre.krro.brush.taper
  "基于像素距离的锥化，通过 :end-dist 指定片段末尾的全局距离。"
  (:require [top.kzre.krro.brush.util :as util]))

(defn total-length [params]
  (if (<= (count params) 1)
    0.0
    (let [pairs (partition 2 1 (map #(vector (:x %) (:y %)) params))]
      (reduce (fn [acc [p1 p2]] (+ acc (util/distance p1 p2))) 0.0 pairs))))

(defn taper-stroke
  [stroke start-px end-px & {:keys [fields end-dist] :or {fields [:radius]}}]
  (let [{:keys [params]} stroke
        params (vec params)    ;; 确保是向量
        n (int (count params))
        start-px (double start-px)
        end-px (double end-px)
        end-dist (double (or end-dist (total-length params)))
        fields (vec fields)]
    (if (and (zero? start-px) (zero? end-px))
      stroke
      (let [reversed-params
            (loop [i (dec n)
                   remain 0.0
                   acc (transient [])]
              (if (neg? i)
                (persistent! acc)
                (let [p (nth params i)
                      dist (- end-dist remain)
                      factor (min 1.0
                                  (if (pos? start-px) (/ dist start-px) 1.0)
                                  (if (pos? end-px)   (/ (- end-dist dist) end-px) 1.0))
                      p' (reduce (fn [m f] (if (contains? m f) (update m f * factor) m))
                                 p fields)
                      seg (if (> i 0)
                            (let [prev-p (nth params (dec i))
                                  dx (- (:x p) (:x prev-p))
                                  dy (- (:y p) (:y prev-p))]
                              (Math/sqrt (+ (* dx dx) (* dy dy))))
                            0.0)]
                  (recur (dec i) (+ remain seg) (conj! acc p')))))
            new-params (vec (reverse reversed-params))]
        (assoc stroke :params new-params)))))

(defn taper-stroke-start
  [stroke start-px & {:keys [fields end-dist] :or {fields [:radius]}}]
  (taper-stroke stroke start-px 0 :fields fields :end-dist end-dist))

(defn taper-stroke-end
  [stroke end-px & {:keys [fields end-dist] :or {fields [:radius]}}]
  (taper-stroke stroke 0 end-px :fields fields :end-dist end-dist))