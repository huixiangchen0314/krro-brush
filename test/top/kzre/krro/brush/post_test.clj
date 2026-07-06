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
        effect {:type :watercolor-edge :intensity 0.8}
        params {:x 1 :y 1}   ;; 中心像素坐标
        result (post/apply-post-effect effect pixel nil edge-dab nil params)]
    (is (<= (first result) 0.8) "Red channel should not increase")
    (is (>= (first result) 0.0) "Red channel should be non-negative")
    (is (= 1.0 (peek result)) "Alpha should remain 1.0")
    (is (every? #(<= 0.0 % 1.0) result) "All components in range")))

(deftest test-watercolor-edge-on-non-edge-pixel
  (let [pixel [0.5 0.5 0.5 1.0]
        effect {:type :watercolor-edge :intensity 0.8}
        params {:x 0 :y 0}   ;; 左上角，alpha较低
        result (post/apply-post-effect effect pixel nil edge-dab nil params)]
    (is (>= (first result) 0.0) "Red should be >= 0")
    (is (<= (first result) 0.5) "Red should not increase beyond original")
    (is (= 1.0 (peek result)) "Alpha stays 1.0")))

(deftest test-watercolor-edge-with-full-alpha
  (let [dab (make-dab-mask 1 1 [1.0])
        pixel [0.2 0.6 0.3 1.0]
        effect {:type :watercolor-edge :intensity 0.5}
        params {:x 0 :y 0}
        result (post/apply-post-effect effect pixel nil dab nil params)]
    ;; 像素alpha=1，效果应该直接返回原像素
    (is (= pixel result))))

(deftest test-watercolor-edge-with-zero-alpha
  (let [dab (make-dab-mask 1 1 [0.0])
        pixel [0.0 0.0 0.0 0.0]
        effect {:type :watercolor-edge :intensity 0.5}
        params {:x 0 :y 0}
        result (post/apply-post-effect effect pixel nil dab nil params)]
    (is (= pixel result))))

(deftest test-paper-texture-basic
  (let [pixel [1.0 1.0 1.0 1.0]
        effect {:type :paper-texture :strength 0.5}
        brush-spec {:texture paper-tex}   ;; 纹理放在 :texture 键中
        params {:x 0 :y 0}  ;; 采样tex(0,0) = 0.2
        result (post/apply-post-effect effect pixel nil nil brush-spec params)]
    ;; 强度0.5，纹理值0.2，factor = 1 - 0.5*0.2 = 0.9
    (is (== 0.9 (first result)) "Red should be 0.9")
    (is (== 0.9 (second result)) "Green should be 0.9")
    (is (== 0.9 (nth result 2)) "Blue should be 0.9")
    (is (== 1.0 (peek result)) "Alpha unchanged")))

(deftest test-paper-texture-clamp
  (let [pixel [0.1 0.1 0.1 1.0]
        effect {:type :paper-texture :strength 1.0}
        brush-spec {:texture paper-tex}
        params {:x 0 :y 1}  ;; tex(0,1) = 0.5, factor = 1 - 1*0.5 = 0.5
        result (post/apply-post-effect effect pixel nil nil brush-spec params)]
    ;; 0.1 * 0.5 = 0.05, 在范围内
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest test-paper-texture-tiling
  (let [pixel [0.5 0.5 0.5 1.0]
        effect {:type :paper-texture :strength 0.3}
        brush-spec {:texture paper-tex}
        params {:x 3 :y 3}  ;; tiling: (3 mod 2, 3 mod 2) = (1,1), tex(1,1)=0.3, factor=1-0.3*0.3=0.91
        result (post/apply-post-effect effect pixel nil nil brush-spec params)]
    ;; 0.5 * 0.91 = 0.455
    (is (< (Math/abs (- 0.455 (first result))) 0.01) "Red should be ~0.455")))