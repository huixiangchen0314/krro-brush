(ns top.kzre.krro.brush.vector-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.vector :as vector])
  (:import (top.kzre.curve.bezier2d Curve)))

(defn- make-line-events [x1 y1 x2 y2 steps pressure]
  (let [dx (/ (- x2 x1) (dec steps))
        dy (/ (- y2 y1) (dec steps))]
    (mapv (fn [i]
            {:x (+ x1 (* dx i))
             :y (+ y1 (* dy i))
             :pressure pressure
             :velocity 0.5
             :timestamp (* 10 i)})
          (range steps))))

(deftest test-generate-vector-stroke-basic
  (let [brush-def {:dynamics {:pressure {:radius {:curve :linear :min 2.0 :max 10.0}}}}
        events (make-line-events 0 0 100 0 5 0.8)
        stroke-data (vector/generate-vector-stroke brush-def events :max-error 1.0 :width-samples 20)]
    (is (map? stroke-data))
    (is (contains? stroke-data :curve))
    (is (contains? stroke-data :width-samples))
    (is (contains? stroke-data :arc-params))
    ;; 曲线对象应为 Java Curve
    (let [curve (:curve stroke-data)]
      (is (instance? Curve curve))
      ;; 至少有两个锚点
      (is (>= (.getPoints curve) 2)))   ;; 注意 .getPoints() 返回 List
    ;; 宽度样本数应为 width-samples+1
    (is (= 21 (count (:width-samples stroke-data))))
    (is (= 21 (count (:arc-params stroke-data))))
    ;; 宽度值应在 [2,10] 内，且都大于 2
    (is (every? #(>= % 2.0) (:width-samples stroke-data)))
    (is (every? #(<= % 10.0) (:width-samples stroke-data)))))

(deftest test-generate-vector-stroke-empty-input
  (let [brush-def {}
        stroke-data (vector/generate-vector-stroke brush-def [] :max-error 1.0 :width-samples 10)]
    (is (map? stroke-data))
    (is (instance? Curve (:curve stroke-data)))
    ;; 应返回退化的宽度样本（全默认值）
    (is (= 11 (count (:width-samples stroke-data))))
    ;; 宽度应为默认 5.0
    (is (every? #(== 5.0 %) (:width-samples stroke-data)))))

(deftest test-generate-vector-stroke-single-point
  (let [brush-def {}
        events [{:x 10 :y 20 :pressure 1.0 :velocity 0.5 :timestamp 0}]
        stroke-data (vector/generate-vector-stroke brush-def events :max-error 1.0 :width-samples 5)]
    (is (map? stroke-data))
    (is (instance? Curve (:curve stroke-data)))
    (is (= 6 (count (:width-samples stroke-data))))
    (is (every? #(== 5.0 %) (:width-samples stroke-data)))))

(deftest test-generate-vector-stroke-dynamics
  ;; 动力学映射控制宽度：压力 0.5 应映射到中间值 6.0 (min 2 max 10)
  (let [brush-def {:dynamics {:pressure {:radius {:curve :linear :min 2.0 :max 10.0}}}}
        events (make-line-events 0 0 100 0 5 0.5)
        stroke-data (vector/generate-vector-stroke brush-def events :max-error 1.0 :width-samples 10)]
    (is (map? stroke-data))
    (is (instance? Curve (:curve stroke-data)))
    ;; 宽度应在 2 到 10 之间，具体值取决于动力学计算（压力 0.5 -> 6.0）
    (is (every? #(>= % 2.0) (:width-samples stroke-data)))
    (is (every? #(<= % 10.0) (:width-samples stroke-data)))
    ;; 至少有一个宽度不等于 5.0（因为动力学生效）
    (is (not (every? #(== 5.0 %) (:width-samples stroke-data))))))