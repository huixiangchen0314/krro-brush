(ns top.kzre.krro.brush.vector.render-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.vector.render :as render]
            [top.kzre.krro.brush.vector.fit :as fit]
            [top.kzre.krro.brush.util :as util]))

;; 辅助：生成一段简单的贝塞尔曲线（直线）
(def straight-segment
  [{:start [10 100] :cp1 [20 100] :cp2 [40 100] :end [50 100]
    :pressure 0.8}])

;; 辅助：生成一条简单的弯曲曲线
(def curved-segments
  (let [pts (mapv (fn [[x y]] {:x x :y y :pressure 0.5 :timestamp 0})
                  [[0 0] [10 10] [20 10] [30 0]])]
    (fit/fit-curve pts)))

;; 恒等线宽函数
(defn- constant-width [w] (fn [_] w))

(deftest test-render-scanline-structure
  (let [dab-size 128
        width-fn (constant-width 4.0)
        result (render/render-vector-scanline {:segments straight-segment} width-fn dab-size)]
    (is (map? result))
    (is (contains? result :data))
    (is (contains? result :width))
    (is (contains? result :height))
    (is (== dab-size (:width result)))
    (is (== dab-size (:height result)))
    ;; 数据应非空，并且至少有一些非零值
    (is (pos? (count (:data result))))
    (is (pos? (reduce + (map #(if (pos? %) 1 0) (:data result)))))))

(deftest test-render-scanline-has-content
  (let [dab-size 64
        width-fn (constant-width 5.0)
        result (render/render-vector-scanline {:segments straight-segment} width-fn dab-size)
        data (:data result)
        ;; 中心点附近应该有非零值
        center-x (quot dab-size 2)
        center-y (quot dab-size 2)
        center-idx (+ center-x (* center-y dab-size))]
    (is (> (aget data center-idx) 0.0) "Center should be drawn")))

(deftest test-render-scanline-antialias
  (let [dab-size 64
        width-fn (constant-width 4.0)
        result (render/render-vector-scanline {:segments straight-segment} width-fn dab-size)
        data (:data result)
        center-x (quot dab-size 2)
        center-y (quot dab-size 2)
        ;; 中心附近的不透明度接近 1
        center-idx (+ center-x (* center-y dab-size))
        center-alpha (aget data center-idx)
        ;; 边缘附近应该有一些半透明像素（验证抗锯齿存在）
        edge-idx (+ (- center-x 5) (* center-y dab-size))
        edge-alpha (aget data edge-idx 0.0)]
    (is (> center-alpha 0.9) "Center opacity should be near 1")
    ;; 边缘至少存在非 0 且非 1 的值（近似检查）
    (let [non-one (some #(and (> % 0.0) (< % 1.0)) (seq data))]
      (is (some? non-one) "There should be antialiased pixels"))))

(deftest test-smooth-width-fn
  (let [segments [{:pressure 0.2} {:pressure 0.8} {:pressure 0.5}]
        width-fn (render/create-smooth-width-fn segments 10.0 1)]
    (is (fn? width-fn))
    (let [w0 (width-fn 0.0)
          w1 (width-fn 0.5)
          w2 (width-fn 1.0)]
      ;; 平滑后的宽度应在合理范围内
      (is (> w0 0.0))
      (is (< w2 10.0))
      ;; 值应当是平滑的，中间值介于两端之间（大致）
      (is (>= w1 (min w0 w2)))
      (is (<= w1 (max w0 w2))))))

(deftest test-render-empty-segments
  (let [dab-size 64
        width-fn (constant-width 5.0)
        result (render/render-vector-scanline {:segments []} width-fn dab-size)]
    ;; 应该返回全零数据
    (is (every? #(== 0.0 %) (:data result)))))

(deftest test-render-curved-segment
  (let [dab-size 128
        width-fn (constant-width 3.0)
        result (render/render-vector-scanline {:segments curved-segments} width-fn dab-size)]
    (is (map? result))
    (is (pos? (reduce + (map #(if (pos? %) 1 0) (:data result))))
        "Curve should produce non-zero pixels")))