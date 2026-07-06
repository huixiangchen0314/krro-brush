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
  "生成矢量笔触的纯几何数据。
   参数：
     brush-spec – 笔刷定义 map，应包含 :dynamics (可选)
     events    – 输入事件序列，至少包含 :x, :y
   选项：
     :max-error      – 曲线拟合误差，默认 4.0
     :width-samples  – 宽度输出采样数，默认 100
   返回：
     :curve         – 贝塞尔曲线 (Curve 对象)
     :width-samples – 等弧长采样的宽度向量 (double[])
     :arc-params    – 对应的弧长参数向量 (double[])"
  [brush-spec events & {:keys [max-error width-samples]
                       :or {max-error 4.0 width-samples 100}}]
  (let [dyn-spec  (:dynamics brush-spec)
        widths    (process-events events dyn-spec)
        xs        (double-array (map :x events))
        ys        (double-array (map :y events))
        result    (VectorStroke/generate xs ys widths max-error width-samples)]
    {:curve         (.getCurve result)
     :width-samples (vec (.getWidthSamples result))
     :arc-params    (vec (.getArcParams result))}))