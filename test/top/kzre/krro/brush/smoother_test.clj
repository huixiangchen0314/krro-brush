(ns top.kzre.krro.brush.smoother-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.smooth :as smoother]
            [top.kzre.krro.brush.stroke :as stroke]))

(defn- make-events [coords]
  (mapv (fn [[i [x y]]]
          {:x x :y y :pressure 0.8 :velocity 0.2
           :timestamp (* 10.0 (inc i))})
        (map-indexed vector coords)))

;; 50 个点的水平直线
(def long-line (make-events (for [x (range 50)] [x 0.0])))

;; 平滑规格（只包含滤波器参数）
(def gaussian-spec {:stabilizer :gaussian :smoothing 5 :sigma 1.5})
(def kalman-spec {:stabilizer :kalman :smoothing 0.01 :measurement-noise 0.1 :process-noise 0.01})
(def cable-spec {:stabilizer :cable :smoothing 0.3})

(deftest test-gaussian-smooth
  (let [result (smoother/smooth long-line gaussian-spec)]
    (is (seq result))
    (is (every? #(contains? % :x) result))
    (is (every? #(contains? % :y) result))
    ;; 滤波后事件数不变
    (is (= (count long-line) (count result)))))

(deftest test-kalman-smooth
  (let [result (smoother/smooth long-line kalman-spec)]
    (is (seq result))
    (is (= (count long-line) (count result)))
    (is (every? #(>= (:x %) 0.0) result))))

(deftest test-cable-smooth
  (let [result (smoother/smooth long-line cable-spec)]
    (is (seq result))
    (is (= (count long-line) (count result)))))


(deftest test-empty-input
  (let [result (smoother/smooth [] gaussian-spec)]
    (is (empty? result))))