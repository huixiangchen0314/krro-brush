(ns top.kzre.krro.brush.mix-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.brush.mix :as mix]
            [top.kzre.krro.brush.protocol :as p]))

(deftest test-default-mixer
  (let [fg [1.0 0.0 0.0 1.0]
        bg [0.0 0.0 0.0 1.0]
        result (p/mix-colors mix/default-mixer fg bg 0.5 :basic)]
    (is (every? #(<= 0.0 % 1.0) result))
    (is (= 4 (count result)))
    ;; 简单验证：红色半透明覆盖黑色，混合后红色通道应 > 0 且 < 1
    (is (> (first result) 0.0))
    (is (< (first result) 1.0))))