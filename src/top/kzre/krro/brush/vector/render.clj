(ns top.kzre.krro.brush.vector.render
  "将贝塞尔曲线光栅化为灰度遮罩，支持可变线宽。"
  (:require [top.kzre.krro.brush.util :as util]))

(defn- evaluate-bezier
  "计算三次贝塞尔曲线上参数 t 处的点。"
  [[P0 P1 P2 P3] t]
  (let [u (- 1 t)]
    (mapv + (mapv #(* u u u %) P0)
          (mapv #(* 3 u u t %) P1)
          (mapv #(* 3 u t t %) P2)
          (mapv #(* t t t %) P3))))

(defn- render-segment
  "将单个贝塞尔段光栅化到灰度数组中。"
  [segment width-fn dab-size]
  (let [data (double-array (* dab-size dab-size) 0.0)
        half (/ dab-size 2.0)
        steps 100  ;; 采样点密度
        points (mapv (fn [i] (evaluate-bezier [(:start segment) (:cp1 segment) (:cp2 segment) (:end segment)]
                                              (/ i (dec steps))))
                     (range steps))
        widths (mapv width-fn (range steps))]
    ;; 简单实现：绘制一系列圆形 dab
    (doseq [i (range steps)]
      (let [pt (nth points i)
            w  (nth widths i)
            radius (/ w 2.0)
            cx (+ (:x pt) half)
            cy (+ (:y pt) half)]
        ;; 遍历 dab 区域内像素，计算距离并添加 alpha
        (doseq [y (range dab-size)
                x (range dab-size)]
          (let [dx (- x cx)
                dy (- y cy)
                dist (Math/sqrt (+ (* dx dx) (* dy dy)))
                alpha (if (>= dist radius)
                        0.0
                        (util/clamp 0.0 1.0 (- 1.0 (/ dist radius))))]
            (when (> alpha 0.0)
              (aset-double data (+ x (* y dab-size))
                           (+ (aget data (+ x (* y dab-size))) alpha)))))))
    data))

(defn render-vector
  "将矢量笔触数据渲染为灰度遮罩。
   vector-data 是 {:segments [...], :pressures [...]}
   width-fn 是 (fn [t] width) 的函数，t 为全局曲线参数 0..1
   返回 {:data [double] :width w :height h} 的 dab 遮罩"
  [vector-data width-fn dab-size]
  (let [segments (:segments vector-data)
        data (double-array (* dab-size dab-size) 0.0)]
    (doseq [seg segments]
      (let [seg-data (render-segment seg width-fn dab-size)]
        ;; 合并到总数据（简单相加，后续可优化混合模式）
        (dotimes [i (* dab-size dab-size)]
          (aset-double data i (min 1.0 (+ (aget data i) (aget seg-data i)))))))
    {:data data :width dab-size :height dab-size}))