(ns top.kzre.krro.brush.vector
  "矢量笔刷：基于动力学处理输入，拟合贝塞尔曲线并生成等弧长宽度采样。
   曲线拟合与宽度采样委托给 top.kzre.krro.brush.VectorStroke。"
  (:require [top.kzre.krro.brush.dynamics :as dynamics])
  (:import (top.kzre.krro.brush VectorStroke)))

(defn- process-events
  "使用动力学规格处理事件序列，返回每个点的宽度列表。"
  [events dynamics-spec]
  (map #(let [params (dynamics/map-dynamics % dynamics-spec)
              width  (get params :radius 5.0)]
          (double width))
       events))

(defn generate-vector-stroke
  [brush-spec events & {:keys [max-error width-samples]
                        :or {max-error 4.0 width-samples 100}}]
  (let [dyn-spec  (:dynamics brush-spec)
        widths    (vec (process-events events dyn-spec))   ;; 强制求值为 vector
        xs        (double-array (mapv :x events))           ;; mapv 立即求值
        ys        (double-array (mapv :y events))
        result    (VectorStroke/generate xs ys (double-array widths) max-error width-samples)]
    {:curve         (.getCurve result)
     :width-samples (vec (.getWidthSamples result))
     :arc-params    (vec (.getArcParams result))}))