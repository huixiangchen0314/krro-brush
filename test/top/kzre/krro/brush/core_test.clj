(ns top.kzre.krro.brush.core-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.core :as core]
            [top.kzre.krro.brush.protocol :as p]
            [top.kzre.krro.brush.mix :as mix]
            [top.kzre.krro.brush.util :as util]))

;; ── 简单的内存画布实现 ICanvas ───────────────────────
(defrecord MockCanvas [width height data]
  p/ICanvas
  (get-pixel [_ x y]
    (when (and (>= x 0) (< x width) (>= y 0) (< y height))
      (let [idx (* 4 (+ x (* y width)))]
        [(aget data idx) (aget data (+ idx 1)) (aget data (+ idx 2)) (aget data (+ idx 3))])))
  (set-pixel! [this x y color]
    (when (and (>= x 0) (< x width) (>= y 0) (< y height))
      (let [idx (* 4 (+ x (* y width)))]
        (aset data idx (double (nth color 0)))
        (aset data (+ idx 1) (double (nth color 1)))
        (aset data (+ idx 2) (double (nth color 2)))
        (aset data (+ idx 3) (double (nth color 3)))))
    this)  ;; 返回自身，可变画布
  (width [_] width)
  (height [_] height))

(defn- make-canvas [w h]
  (->MockCanvas w h (double-array (* w h 4) 0.0)))

(defn- canvas-has-content? [canvas]
  (let [data (:data canvas)
        n (alength data)]
    (some #(> (aget data %) 0.0) (range n))))

;; ── 辅助输入事件 ──────────────────────────────────────
(defn- make-line [x1 y1 x2 y2 steps]
  (let [dx (/ (- x2 x1) (dec steps))
        dy (/ (- y2 y1) (dec steps))]
    (mapv (fn [i]
            {:x (+ x1 (* dx i))
             :y (+ y1 (* dy i))
             :pressure 0.8
             :velocity 0.5
             :timestamp (* 10 i)})
          (range steps))))

;; ── 集成测试 ──────────────────────────────────────────
(deftest test-render-stroke-basic
  (let [canvas (make-canvas 200 200)
        ;; 简单的硬边圆形笔刷
        brush-def {:dab {:type :circle :mask-type :hard :radius 8.0}
                   :color {:color [0.0 0.0 0.0 1.0] :blend-model :basic}
                   :dynamics {}
                   :stroke {:stabilizer :gaussian :smoothing 1 :spacing 0.2 :dab {:radius 8.0}}
                   :post nil}
        events (make-line 50 100 150 100 10)
        result (core/render-stroke brush-def canvas events)]
    ;; 画布上应该出现笔触
    (is (canvas-has-content? (:canvas result)))
    ;; 脏矩形不应为空
    (is (seq (:dirty-rects result)))))

(deftest test-render-stroke-with-dynamics
  (let [canvas (make-canvas 200 200)
        brush-def {:dab {:type :circle :mask-type :soft :radius 5.0}
                   :color {:color [0.0 0.0 1.0 1.0] :blend-model :basic}
                   :dynamics {:pressure {:radius {:curve :linear :min 2.0 :max 10.0}}}
                   :stroke {:stabilizer :gaussian :smoothing 1 :spacing 0.2 :dab {:radius 5.0}}
                   :post nil}
        events (make-line 50 100 150 100 10)
        result (core/render-stroke brush-def canvas events)]
    (is (canvas-has-content? (:canvas result)))
    (is (seq (:dirty-rects result)))))

(deftest test-render-stroke-empty-input
  (let [canvas (make-canvas 100 100)
        brush-def {:dab {:type :circle :mask-type :soft :radius 5.0}
                   :color {:color [0 0 0 1] :blend-model :basic}
                   :dynamics {}
                   :stroke {:stabilizer :gaussian :smoothing 1 :spacing 0.2 :dab {:radius 5.0}}
                   :post nil}
        result (core/render-stroke brush-def canvas [])]
    ;; 空输入不应该崩溃，画布应保持不变
    (is (not (canvas-has-content? (:canvas result))))
    (is (empty? (:dirty-rects result)))))

(deftest test-render-stroke-colors-in-range
  (let [canvas (make-canvas 100 100)
        brush-def {:dab {:type :circle :mask-type :soft :radius 5.0}
                   :color {:color [0.5 0.5 0.5 0.5] :blend-model :basic}
                   :dynamics {}
                   :stroke {:stabilizer :gaussian :smoothing 1 :spacing 0.2 :dab {:radius 5.0}}
                   :post nil}
        events (make-line 30 50 70 50 5)
        result (core/render-stroke brush-def canvas events)
        data (:data (:canvas result))]
    ;; 所有颜色分量应在 0..1 范围内（不会出现 NaN 或无穷）
    (doseq [i (range (alength data))]
      (let [v (aget data i)]
        (is (<= 0.0 v 1.0) (str "Value out of range at index " i ": " v))))))