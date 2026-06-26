(ns top.kzre.krro.brush.dynamics-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.dynamics :as dynamics]))

(deftest test-apply-curve-linear
  (is (== 0.5 (dynamics/apply-curve 0.5 :linear)))
  (is (== 0.0 (dynamics/apply-curve 0.0 :linear)))
  (is (== 1.0 (dynamics/apply-curve 1.0 :linear))))

(deftest test-apply-curve-sigmoid
  ;; sigmoid(0) = 0.5
  (is (== 0.5 (dynamics/apply-curve 0.0 :sigmoid)))
  ;; sigmoid(0.5) ≈ 0.622459… > 0.5
  (is (> (dynamics/apply-curve 0.5 :sigmoid) 0.5))
  ;; sigmoid(1.0) ≈ 0.731058… > 0.5
  (is (> (dynamics/apply-curve 1.0 :sigmoid) 0.5)))

(deftest test-apply-curve-quadratic
  (is (== 0.25 (dynamics/apply-curve 0.5 :quadratic)))
  (is (== 0.0 (dynamics/apply-curve 0.0 :quadratic)))
  (is (== 1.0 (dynamics/apply-curve 1.0 :quadratic))))

(deftest test-apply-curve-cubic
  (is (== 0.125 (dynamics/apply-curve 0.5 :cubic)))
  (is (== 0.0 (dynamics/apply-curve 0.0 :cubic)))
  (is (== 1.0 (dynamics/apply-curve 1.0 :cubic))))

(deftest test-apply-curve-sqrt
  (is (> (dynamics/apply-curve 0.25 :sqrt) 0.25)))

(deftest test-apply-curve-inv-sigmoid
  (let [result (dynamics/apply-curve 0.5 :inv-sigmoid)]
    (is (< result 0.5))
    (is (> result 0.0))))

(deftest test-apply-curve-default
  (is (== 0.5 (dynamics/apply-curve 0.5 :unknown-curve))))

;; ── 映射测试 ──────────────────────────────────────────

(deftest test-map-dynamics-pressure-to-radius
  (let [dab-base {:x 100.0 :y 200.0}
        dyn-spec {:pressure {:radius {:curve :linear :min 5.0 :max 15.0}}}
        input-event {:pressure 0.5 :velocity 1.0}
        result (dynamics/map-dynamics dab-base dyn-spec input-event)]
    (is (== 10.0 (:radius result)))))

(deftest test-map-dynamics-preserves-other-keys
  (let [dab-base {:x 100.0 :y 200.0 :extra :keep}
        dyn-spec {:pressure {:radius {:curve :linear :min 5.0 :max 15.0}}}
        input-event {:pressure 0.5}
        result (dynamics/map-dynamics dab-base dyn-spec input-event)]
    (is (= :keep (:extra result)))
    (is (= 100.0 (:x result)))
    (is (= 200.0 (:y result)))))

(deftest test-map-dynamics-multiple-parameters
  (let [dab-base {:x 0 :y 0}
        dyn-spec {:pressure {:radius {:curve :linear :min 1.0 :max 10.0}
                             :opacity {:curve :sigmoid :min 0.2 :max 1.0}}}
        input-event {:pressure 0.3}
        result (dynamics/map-dynamics dab-base dyn-spec input-event)]
    (is (some? (:radius result)))
    (is (some? (:opacity result)))))

(deftest test-map-dynamics-missing-sensor-defaults-to-1
  (let [dab-base {:x 0 :y 0}
        dyn-spec {:pressure {:radius {:curve :linear :min 5.0 :max 15.0}}}
        input-event {:velocity 0.5}
        result (dynamics/map-dynamics dab-base dyn-spec input-event)]
    ;; 默认压力为 1.0 → 半径 = max = 15.0
    (is (== 15.0 (:radius result)))))

(deftest test-map-dynamics-velocity
  (let [dab-base {:x 0 :y 0}
        dyn-spec {:velocity {:spacing {:curve :linear :min 0.5 :max 1.5}}}
        input-event {:velocity 0.0}
        result (dynamics/map-dynamics dab-base dyn-spec input-event)]
    (is (== 0.5 (:spacing result)))))

(deftest test-map-dynamics-extreme-values
  (let [dab-base {:x 0}
        dyn-spec {:pressure {:radius {:curve :linear :min -5.0 :max 25.0}}}
        input-event {:pressure 0.5}
        result (dynamics/map-dynamics dab-base dyn-spec input-event)]
    ;; 不崩溃，返回值应为 (-5 + 0.5*30) = 10.0
    (is (== 10.0 (:radius result)))))