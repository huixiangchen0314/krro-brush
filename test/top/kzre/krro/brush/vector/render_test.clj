(ns top.kzre.krro.brush.vector.render-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.vector.render :as render]
            [top.kzre.krro.brush.vector.fit :as fit]
            [top.kzre.krro.brush.util :as util]))

(defn- constant-width [w] (fn [_] w))

(def straight-segment
  [{:start [10 20] :cp1 [20 20] :cp2 [40 20] :end [50 20] :pressure 0.8}])

(deftest test-render-scanline-structure
  (let [dab-size 64
        width-fn (constant-width 4.0)
        result (render/render-vector-scanline {:segments straight-segment} width-fn dab-size)]
    (is (map? result))
    (is (contains? result :data))
    (is (contains? result :width))
    (is (contains? result :height))
    (is (== dab-size (:width result)))
    (is (== dab-size (:height result)))
    (let [data (:data result)]
      (is (pos? (alength data)))
      (is (pos? (reduce + (map #(if (pos? %) 1.0 0.0) (seq data))))))))

(deftest test-render-scanline-has-content
  (let [dab-size 64
        width-fn (constant-width 5.0)
        result (render/render-vector-scanline {:segments straight-segment} width-fn dab-size)
        data (:data result)
        ;; 线段 y≈20，在 y=20 处且靠近中点 x≈30 的位置应被绘制
        x 30
        y 20
        idx (+ x (* y dab-size))]
    (is (> (aget data idx) 0.0) "Point on line should be drawn")))

(deftest test-render-scanline-antialias
  (let [dab-size 64
        width-fn (constant-width 4.0)       ;; 半宽 2.0, 边缘在 y≈18 和 y≈22
        result (render/render-vector-scanline {:segments straight-segment} width-fn dab-size)
        data (:data result)
        ;; 检查靠近上边缘的像素：y=18 附近应存在半透明
        y 18
        xs (range 15 46)                    ;; 线段 x 范围 10..50, 检查中间部分
        pixels (map #(aget data (+ % (* y dab-size))) xs)
        non-one (some #(and (> % 0.0) (< % 1.0)) pixels)]
    (is (some? non-one) "There should be antialiased pixels near the edge")))

(deftest test-smooth-width-fn
  (let [segments [{:pressure 0.2} {:pressure 0.8} {:pressure 0.5}]
        width-fn (render/create-smooth-width-fn segments 10.0 1)]
    (is (fn? width-fn))
    (let [w0 (width-fn 0.0)
          w1 (width-fn 0.5)
          w2 (width-fn 1.0)]
      (is (> w0 0.0))
      (is (< w2 10.0))
      (is (>= w1 (min w0 w2)))
      (is (<= w1 (max w0 w2))))))

(deftest test-render-empty-segments
  (let [dab-size 64
        width-fn (constant-width 5.0)
        result (render/render-vector-scanline {:segments []} width-fn dab-size)]
    (is (every? #(== 0.0 %) (:data result)))))

(deftest test-render-curved-segment
  (let [pts (mapv (fn [[x y]] {:x x :y y :pressure 0.5 :timestamp 0})
                  [[0 0] [10 10] [20 10] [30 0]])
        curved-segments (fit/fit-curve pts)
        dab-size 64
        width-fn (constant-width 3.0)
        result (render/render-vector-scanline {:segments curved-segments} width-fn dab-size)]
    (is (map? result))
    (is (pos? (reduce + (map #(if (pos? %) 1 0) (:data result))))
        "Curve should produce non-zero pixels")))