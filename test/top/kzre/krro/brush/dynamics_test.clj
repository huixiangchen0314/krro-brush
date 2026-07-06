(ns top.kzre.krro.brush.dynamics-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.dynamics :as dynamics])
  (:import [top.kzre.curve.bezier2d Bezier2D]))

;; ── apply-curve 测试 ─────────────────────────────────
(deftest test-apply-curve-linear
  (is (== 0.5 (dynamics/apply-curve 0.5 :linear nil)))
  (is (== 0.0 (dynamics/apply-curve 0.0 :linear nil)))
  (is (== 1.0 (dynamics/apply-curve 1.0 :linear nil))))

(deftest test-apply-curve-sigmoid
  (is (== 0.5 (dynamics/apply-curve 0.0 :sigmoid nil)))
  (is (> (dynamics/apply-curve 0.5 :sigmoid nil) 0.5))
  (is (> (dynamics/apply-curve 1.0 :sigmoid nil) 0.5)))

(deftest test-apply-curve-quadratic
  (is (== 0.25 (dynamics/apply-curve 0.5 :quadratic nil)))
  (is (== 0.0 (dynamics/apply-curve 0.0 :quadratic nil)))
  (is (== 1.0 (dynamics/apply-curve 1.0 :quadratic nil))))

(deftest test-apply-curve-cubic
  (is (== 0.125 (dynamics/apply-curve 0.5 :cubic nil)))
  (is (== 0.0 (dynamics/apply-curve 0.0 :cubic nil)))
  (is (== 1.0 (dynamics/apply-curve 1.0 :cubic nil))))

(deftest test-apply-curve-sqrt
  (is (> (dynamics/apply-curve 0.25 :sqrt nil) 0.25)))

(deftest test-apply-curve-inv-sigmoid
  (let [result (dynamics/apply-curve 0.5 :inv-sigmoid nil)]
    (is (< result 0.5))
    (is (> result 0.0))))

(deftest test-apply-curve-default
  (is (== 0.5 (dynamics/apply-curve 0.5 :unknown-curve nil))))

(deftest test-apply-curve-bezier
  (let [;; 构造一条非线性的贝塞尔曲线 (0,0)→(1,1) 但中间拱起
        curve (Bezier2D/fit (double-array [0 0.5 50 1.0 101])
                            (double-array [0 20 36 1.0 8])
                            0.01 10)]
    ;; 端点测试
    (is (== 0.0 (dynamics/apply-curve 0.0 :bezier {:bezier-curve curve})))
    (is (== 8.0 (dynamics/apply-curve 25 :bezier {:bezier-curve curve})))
    ;; 中间值应偏离 0.5
    (let [mid (dynamics/apply-curve 0.5 :bezier {:bezier-curve curve})]
      (is (> mid 30))
      (is (< mid 40)))))

(deftest test-apply-curve-lookup
  (is (== 0.0 (dynamics/apply-curve 0.0 :lookup {:table [0.0 1.0]})))
  (is (== 1.0 (dynamics/apply-curve 1.0 :lookup {:table [0.0 1.0]})))
  (is (== 0.5 (dynamics/apply-curve 0.5 :lookup {:table [0.0 1.0]})))
  ;; 不对称表：x=0.5 时不应等于 0.5
  (let [result (dynamics/apply-curve 0.5 :lookup {:table [0.0 0.9 1.0]})]
    (is (> result 0.5))
    (is (< result 1.0))))

;; ── map-dynamics 测试 ────────────────────────────────
(deftest test-map-dynamics-pressure-to-radius
  (let [event {:x 100.0 :y 200.0 :pressure 0.5}
        dyn-spec {:pressure {:radius {:curve :linear :min 5.0 :max 15.0}}}
        result (dynamics/map-dynamics event dyn-spec)]
    (is (== 10.0 (:radius result)))
    ;; 原始坐标保留
    (is (== 100.0 (:x result)))
    (is (== 200.0 (:y result)))))

(deftest test-map-dynamics-preserves-other-keys
  (let [event {:x 100.0 :y 200.0 :extra :keep :pressure 0.5}
        dyn-spec {:pressure {:radius {:curve :linear :min 5.0 :max 15.0}}}
        result (dynamics/map-dynamics event dyn-spec)]
    (is (= :keep (:extra result)))))

(deftest test-map-dynamics-multiple-parameters
  (let [event {:x 0 :y 0 :pressure 0.3}
        dyn-spec {:pressure {:radius {:curve :linear :min 1.0 :max 10.0}
                             :opacity {:curve :sigmoid :min 0.2 :max 1.0}}}
        result (dynamics/map-dynamics event dyn-spec)]
    (is (some? (:radius result)))
    (is (some? (:opacity result)))))

(deftest test-map-dynamics-missing-sensor-defaults-to-1
  (let [event {:x 0 :y 0 :velocity 0.5}  ; 没有 pressure
        dyn-spec {:pressure {:radius {:curve :linear :min 5.0 :max 15.0}}}
        result (dynamics/map-dynamics event dyn-spec)]
    ;; pressure 默认为 1.0，映射到 max
    (is (== 15.0 (:radius result)))))

(deftest test-map-dynamics-velocity
  (let [event {:x 0 :y 0 :velocity 0.0}
        dyn-spec {:velocity {:spacing {:curve :linear :min 0.5 :max 1.5}}}
        result (dynamics/map-dynamics event dyn-spec)]
    (is (== 0.5 (:spacing result)))))

(deftest test-map-dynamics-extreme-values
  (let [event {:x 0 :pressure 0.5}
        dyn-spec {:pressure {:radius {:curve :linear :min -5.0 :max 25.0}}}
        result (dynamics/map-dynamics event dyn-spec)]
    (is (== 10.0 (:radius result)))))

;; ── sensor-value 测试（已去除 :input-event 包装） ─────
(deftest test-sensor-tilt-normalization
  ;; 无倾斜时，默认值为 0.5
  (is (== 0.5 (dynamics/sensor-value :tilt-x {})))
  (is (== 0.5 (dynamics/sensor-value :tilt-y {})))
  ;; 有倾斜时应在 0~1 之间
  (let [event {:tilt {:x 30.0 :y -20.0}}
        tx (dynamics/sensor-value :tilt-x event)
        ty (dynamics/sensor-value :tilt-y event)]
    (is (> tx 0.5) "Positive X tilt should be > 0.5")
    (is (< ty 0.5) "Negative Y tilt should be < 0.5")))

(deftest test-sensor-rotation-normalization
  (is (== 0.0 (dynamics/sensor-value :rotation {:rotation 0.0})))
  (is (== 0.25 (dynamics/sensor-value :rotation {:rotation 90.0})))
  (is (== 0.5 (dynamics/sensor-value :rotation {:rotation 180.0})))
  (is (== 0.75 (dynamics/sensor-value :rotation {:rotation 270.0}))))