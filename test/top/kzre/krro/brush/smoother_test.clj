(ns top.kzre.krro.brush.smoother-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.smoother :as smoother]))

(defn- make-events [coords]
  (mapv (fn [[i [x y]]]
          {:x x :y y :pressure 0.8 :velocity 0.2
           :timestamp (* 10.0 (inc i))})
        (map-indexed vector coords)))

;; 足够长的直线，确保产生大量 dab
(def long-line (make-events (for [x (range 50)] [x 0.0])))

(def taper-spec
  {:stabilizer :gaussian
   :smoothing 3
   :spacing 0.05           ;; 步长 = 0.05 * 10 = 0.5，产生约 99 个 dab
   :dab {:radius 5.0}
   :taper-start 0.2
   :taper-end 0.2})

(def plain-spec
  {:stabilizer :gaussian
   :smoothing 3
   :spacing 0.05
   :dab {:radius 5.0}})

(deftest test-gaussian-smooth
  (let [result (smoother/smooth long-line plain-spec)]
    (is (seq result))
    (is (every? #(contains? % :x) result))
    (is (every? #(contains? % :y) result))
    (is (<= (count result) (count long-line)))))

(deftest test-kalman-smooth
  (let [result (smoother/smooth long-line
                                {:stabilizer :kalman
                                 :smoothing 0.01
                                 :measurement-noise 0.1
                                 :spacing 0.05
                                 :dab {:radius 5.0}})]
    (is (seq result))
    (is (<= (count result) (count long-line)))
    (is (every? #(>= (:x %) 0.0) result))))

(deftest test-cable-smooth
  (let [result (smoother/smooth long-line
                                {:stabilizer :cable
                                 :smoothing 0.5
                                 :spacing 0.05
                                 :dab {:radius 5.0}})]
    (is (seq result))
    (is (<= (count result) (count long-line)))))

(deftest test-taper
  (let [result (smoother/smooth long-line taper-spec)]
    (is (seq result))
    (is (every? #(contains? % :taper) result))
    (is (> (count result) 4) "Need enough dabs for taper to be visible")
    (let [first-taper (:taper (first result))
          last-taper  (:taper (last result))]
      (is (< first-taper 1.0) "First dab should be tapered")
      (is (< last-taper 1.0)  "Last dab should be tapered")
      (let [mid-idx (quot (count result) 2)
            mid-taper (:taper (nth result mid-idx))]
        (is (== 1.0 mid-taper) "Middle dab should have full opacity")))))

(deftest test-empty-input
  (let [result (smoother/smooth [] taper-spec)]
    (is (empty? result))))