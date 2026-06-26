(ns top.kzre.krro.brush.post-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.post :as post]))

(defn- make-dab-mask [width height data-vec]
  {:width width :height height :data (double-array data-vec)})

(def edge-dab
  (make-dab-mask 3 3
                 [0.1 0.2 0.1
                  0.2 0.5 0.2
                  0.1 0.2 0.1]))

(def paper-tex
  {:width 2 :height 2
   :data (double-array [0.2 0.8
                        0.5 0.3])})

(deftest test-watercolor-edge-on-edge-pixel
  (let [pixel [0.8 0.2 0.2 1.0]
        result (post/apply-watercolor-edge pixel edge-dab 1 1 0.5 0.8)]
    (is (<= (first result) 0.8))
    (is (>= (first result) 0.0))
    (is (= 1.0 (peek result)))
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest test-watercolor-edge-on-non-edge-pixel
  (let [pixel [0.5 0.5 0.5 1.0]
        result (post/apply-watercolor-edge pixel edge-dab 0 0 0.1 0.8)]
    (is (>= (first result) 0.0))
    (is (<= (first result) 0.5))
    (is (= 1.0 (peek result)))))

(deftest test-watercolor-edge-with-full-alpha
  (let [dab (make-dab-mask 1 1 [1.0])
        pixel [0.2 0.6 0.3 1.0]
        result (post/apply-watercolor-edge pixel dab 0 0 1.0 0.5)]
    (is (= pixel result))))

(deftest test-watercolor-edge-with-zero-alpha
  (let [dab (make-dab-mask 1 1 [0.0])
        pixel [0.0 0.0 0.0 0.0]
        result (post/apply-watercolor-edge pixel dab 0 0 0.0 0.5)]
    (is (= pixel result))))

(deftest test-paper-texture-basic
  (let [pixel [1.0 1.0 1.0 1.0]
        result (post/apply-paper-texture pixel paper-tex 0 0 0.5)]
    (is (== 0.9 (first result)))
    (is (== 0.9 (second result)))
    (is (== 0.9 (nth result 2)))
    (is (== 1.0 (peek result)))))

(deftest test-paper-texture-clamp
  (let [pixel [0.1 0.1 0.1 1.0]
        result (post/apply-paper-texture pixel paper-tex 0 1 1.0)]
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest test-paper-texture-tiling
  (let [pixel [0.5 0.5 0.5 1.0]
        result (post/apply-paper-texture pixel paper-tex 3 3 0.3)]
    (is (< (Math/abs (- 0.455 (first result))) 0.01))))