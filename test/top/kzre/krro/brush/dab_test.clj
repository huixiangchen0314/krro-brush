(ns top.kzre.krro.brush.dab-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.dab :as dab]))

(def default-params {:radius 10.0})
(def circle-spec {:dab {:type :circle :mask-type :soft}})          ;; 包装在 :dab 中
(def default-splatter-spec {:dab {:type :splatter :splatter-count 15 :mask-type :soft}})

(deftest test-generate-circle-dab-structure
  (let [dab (dab/generate-dab circle-spec default-params)]
    (is (map? dab))
    (is (contains? dab :data))
    (is (contains? dab :width))
    (is (contains? dab :height))
    (is (== (:width dab) (:height dab)))
    (is (pos? (:width dab)))))

(deftest test-circle-dab-center-opaque
  (let [dab (dab/generate-dab {:dab {:type :circle :mask-type :hard}} {:radius 10.0})
        size (:width dab)
        center (quot size 2)
        idx (+ center (* center size))
        alpha (aget (:data dab) idx)]
    (is (== 1.0 alpha))))

(deftest test-circle-dab-edge-transparent
  (let [dab (dab/generate-dab {:dab {:type :circle :mask-type :hard}} {:radius 10.0})
        size (:width dab)
        edge-alpha (aget (:data dab) 0)]  ;; 角落肯定在外面
    (is (== 0.0 edge-alpha))))

(deftest test-soft-mask-has-gradient
  (let [dab-soft (dab/generate-dab {:dab {:type :circle :mask-type :soft}} {:radius 10.0})
        dab-hard (dab/generate-dab {:dab {:type :circle :mask-type :hard}} {:radius 10.0})
        center (quot (:width dab-soft) 2)
        ;; 取半径一半处的像素
        half-r (int (+ center 5))
        idx (+ half-r (* half-r (:width dab-soft)))
        soft-alpha (aget (:data dab-soft) idx)
        hard-alpha (aget (:data dab-hard) idx)]
    (is (> soft-alpha 0.0))
    (is (== hard-alpha 1.0))   ;; 硬边在此处仍在内部
    (is (< soft-alpha 1.0))))

(deftest test-gaussian-mask
  (let [dab (dab/generate-dab {:dab {:type :circle :mask-type :gaussian}} {:radius 10.0})
        size (:width dab)
        center (quot size 2)
        ;; 中心附近接近 1
        alpha-center (aget (:data dab) (+ center (* center size)))]
    (is (> alpha-center 0.9))))

(deftest test-ellipse-dab
  (let [dab (dab/generate-dab {:dab {:type :ellipse :mask-type :soft}}
                              {:radius 10.0 :radius-x 15.0 :radius-y 8.0 :angle 30.0})]
    (is (map? dab))
    (is (pos? (:width dab)))))

(deftest test-star-dab
  (let [dab (dab/generate-dab {:dab {:type :star :points 5 :inner-ratio 0.4 :mask-type :soft}}
                              {:radius 10.0})]
    (is (map? dab))
    (is (pos? (:width dab)))))

(deftest test-polygon-dab
  (let [dab (dab/generate-dab {:dab {:type :polygon :sides 6 :mask-type :soft}}
                              {:radius 10.0 :angle 0.0})]
    (is (map? dab))
    (is (pos? (:width dab)))))

(deftest test-image-dab-fallback
  ;; 无图像数据时回退到圆形
  (let [dab (dab/generate-dab {:dab {:type :image :mask-type :soft}} {:radius 10.0})]
    (is (map? dab))
    (is (pos? (:width dab)))))

(deftest test-custom-dab
  (let [f (fn [x y] (+ (* x x) (* y y)))  ;; 径向渐变
        dab (dab/generate-dab {:dab {:type :custom :custom-fn f}} {:radius 10.0})]
    (is (map? dab))
    ;; 中心值应接近 0（自定义函数在中心为 0）
    (let [size (:width dab)
          center (quot size 2)
          idx (+ center (* center size))]
      (is (< (aget (:data dab) idx) 0.1)))))

(deftest test-dual-dab
  (let [primary-spec {:type :circle :mask-type :hard}
        secondary-spec {:type :circle :mask-type :soft}
        dab (dab/generate-dab {:dab {:type :dual :primary primary-spec :secondary secondary-spec :dual-blend :multiply}}
                              {:radius 10.0})]
    (is (map? dab))
    (is (pos? (:width dab)))))

(deftest test-splatter-dab
  (let [dab (dab/generate-dab default-splatter-spec {:radius 10.0})]
    (is (map? dab))
    ;; 至少应该有一些非零像素
    (is (pos? (reduce + (seq (:data dab)))))))

(deftest test-texture-stamp-dab
  (let [texture {:width 4 :height 4 :data (double-array [1 0 0 0  0 1 0 0  0 0 1 0  0 0 0 1])}
        dab (dab/generate-dab {:dab {:type :texture-stamp :texture-data texture :mask-type :soft}} {:radius 10.0})]
    (is (map? dab))
    (is (pos? (:width dab)))))

(deftest test-dab-cache
  ;; 缓存功能：多次调用应返回相同对象（引用相等）？
  ;; 由于我们使用的是 atom 缓存，连续调用可能返回同一个对象，但这里仅验证数据一致
  (let [dab1 (dab/generate-dab circle-spec default-params)
        dab2 (dab/generate-dab circle-spec default-params)]
    (is (= (seq (:data dab1)) (seq (:data dab2))))
    (is (= (:width dab1) (:width dab2)))))

(deftest test-splatter-reproducible
  ;; 两次使用相同种子应产生完全相同的 dab
  (let [spec (assoc-in default-splatter-spec [:dab :seed] 42)   ;; 注意路径是 [:dab :seed]
        dab1 (dab/generate-dab spec default-params)
        dab2 (dab/generate-dab spec default-params)]
    (is (= (seq (:data dab1)) (seq (:data dab2)))))
  ;; 不同种子应产生不同的 dab
  (let [spec1 (assoc-in default-splatter-spec [:dab :seed] 123)
        spec2 (assoc-in default-splatter-spec [:dab :seed] 456)
        dab1 (dab/generate-dab spec1 default-params)
        dab2 (dab/generate-dab spec2 default-params)]
    (is (not= (seq (:data dab1)) (seq (:data dab2))))))

(deftest test-edge-cases
  ;; 零半径
  (let [dab (dab/generate-dab circle-spec {:radius 0.0})]
    (is (map? dab))
    (is (== 0 (:width dab))))
  ;; 超大半径不会崩溃
  (let [dab (dab/generate-dab circle-spec {:radius 500.0})]
    (is (map? dab))
    (is (pos? (:width dab)))))