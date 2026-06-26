(ns top.kzre.krro.brush.vector.fit-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.vector.fit :as fit]
            [top.kzre.krro.brush.util :as util]))

(defn- make-points [coords]
  (mapv (fn [[i [x y]]]
          {:x x :y y :pressure 0.8 :velocity 0.2
           :timestamp (* 10.0 (inc i))})
        (map-indexed vector coords)))

(def straight-line
  (make-points [[0 0] [10 0] [20 0] [30 0] [40 0]]))

(def curve-points
  (make-points [[0 0] [10 5] [20 10] [30 5] [40 0]]))

(def empty-points [])

(deftest test-fit-straight-line
  (let [result (fit/fit-curve straight-line)]
    (is (vector? result))
    (is (pos? (count result)))
    (doseq [seg result]
      (is (contains? seg :start))
      (is (contains? seg :cp1))
      (is (contains? seg :cp2))
      (is (contains? seg :end))
      (is (contains? seg :pressure)))))

(deftest test-fit-curve
  (let [result (fit/fit-curve curve-points)]
    (is (vector? result))
    (is (pos? (count result)))
    (is (>= (count result) 1))))

(deftest test-fit-empty
  (let [result (fit/fit-curve empty-points)]
    (is (= [] result))))

(deftest test-adaptive-segmentation
  (let [sharp-points (make-points [[0 0] [10 0] [20 0] [30 10] [40 10] [50 10]])
        result (fit/fit-curve sharp-points 1.0)]
    (is (vector? result))
    (is (> (count result) 1) "Sharp corner should cause segmentation")))

(deftest test-fitting-accuracy
  (let [points straight-line
        result (fit/fit-curve points)
        first-seg (first result)
        bezier [(:start first-seg) (:cp1 first-seg) (:cp2 first-seg) (:end first-seg)]
        start-pt (fit/evaluate-bezier bezier 0.0)
        end-pt   (fit/evaluate-bezier bezier 1.0)]
    (is (< (util/distance start-pt [(:x (first points)) (:y (first points))]) 1e-6))
    (is (< (util/distance end-pt   [(:x (last points)) (:y (last points))]) 1e-6))))

(deftest test-pressure-retained
  (let [result (fit/fit-curve straight-line)]
    (doseq [seg result]
      (is (number? (:pressure seg)))
      (is (>= (:pressure seg) 0.0))
      (is (<= (:pressure seg) 1.0)))))