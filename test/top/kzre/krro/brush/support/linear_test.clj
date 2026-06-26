(ns top.kzre.krro.brush.support.linear-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.support.linear :as linear]))

(deftest test-matrix-mult
  (testing "2x2 matrix * vector"
    (is (= [5.0 11.0] (linear/matrix-mult [[1.0 2.0] [3.0 4.0]] [1.0 2.0]))))
  (testing "3x3 matrix * vector"
    (is (= [14.0 32.0 50.0] (linear/matrix-mult [[1.0 2.0 3.0]
                                                 [4.0 5.0 6.0]
                                                 [7.0 8.0 9.0]]
                                                [1.0 2.0 3.0])))))

(deftest test-matrix-2x2-inv
  (let [A [[4.0 7.0] [2.0 6.0]]
        inv (linear/matrix-2x2-inv A)]
    (is (some? inv))
    (let [I (linear/matrix-mult A (linear/matrix-mult inv [1.0 0.0]))]
      (is (< (Math/abs (- (first I) 1.0)) 1e-10))
      (is (< (Math/abs (second I)) 1e-10))))
  (testing "singular matrix returns nil"
    (is (nil? (linear/matrix-2x2-inv [[0.0 0.0] [0.0 0.0]])))))

(deftest test-matrix-3x3-inv
  (let [A [[1.0 2.0 3.0] [0.0 1.0 4.0] [5.0 6.0 0.0]]
        inv (linear/matrix-3x3-inv A)]
    (is (some? inv))
    (let [v (linear/matrix-mult inv [1.0 0.0 0.0])
          check (linear/matrix-mult A v)]
      (is (< (Math/abs (- (first check) 1.0)) 1e-10))
      (is (< (Math/abs (second check)) 1e-10))
      (is (< (Math/abs (nth check 2)) 1e-10)))))

(deftest test-solve-2x2
  (let [A [[4.0 7.0] [2.0 6.0]]
        b [1.0 2.0]
        x (linear/solve-2x2 A b)]
    (is (some? x))
    (let [Ax (linear/matrix-mult A x)]
      (is (< (Math/abs (- (first Ax) (first b))) 1e-10))
      (is (< (Math/abs (- (second Ax) (second b))) 1e-10)))))

(deftest test-solve-3x3
  (let [A [[1.0 2.0 3.0] [0.0 1.0 4.0] [5.0 6.0 0.0]]
        b [1.0 2.0 3.0]
        x (linear/solve-3x3 A b)]
    (is (some? x))
    (let [Ax (linear/matrix-mult A x)]
      (is (< (Math/abs (- (first Ax) (first b))) 1e-10))
      (is (< (Math/abs (- (second Ax) (second b))) 1e-10))
      (is (< (Math/abs (- (nth Ax 2) (nth b 2))) 1e-10)))))