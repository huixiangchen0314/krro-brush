(ns top.kzre.krro.brush.post
  "后处理器：水彩边缘、纸纹合成等辅助函数。"
  (:require [top.kzre.krro.brush.util :as util]))

(defn apply-watercolor-edge
  "对单个像素应用水彩边缘加深效果。
   dab-mask: dab遮罩 {:data [double] :width w :height h}
   px, py: 当前像素在遮罩中的坐标
   alpha: 当前像素遮罩透明度
   intensity: 加深强度 0-1
   返回修改后的颜色 [r g b a]"
  [color dab-mask px py alpha intensity]
  (if (and (> alpha 0.01) (< alpha 0.99)) ;; 只在边缘区域
    (let [w (:width dab-mask) h (:height dab-mask)
          data (:data dab-mask)
          ;; 简单梯度近似：与相邻像素差
          left   (if (> px 0) (aget data (+ (dec px) (* py w))) alpha)
          right  (if (< px (dec w)) (aget data (+ (inc px) (* py w))) alpha)
          up     (if (> py 0) (aget data (+ px (* (dec py) w))) alpha)
          down   (if (< py (dec h)) (aget data (+ px (* (inc py) w))) alpha)
          ;; 梯度幅值
          grad (Math/sqrt (+ (Math/pow (- right left) 2) (Math/pow (- down up) 2)))
          edge-factor (util/clamp 0.0 1.0 (* grad 5.0 intensity))
          r (nth color 0) g (nth color 1) b (nth color 2) a (nth color 3)
          luminance (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))
          ;; 向黑色混合
          darkened (mapv #(util/lerp % 0.0 edge-factor) [r g b])
          ;; 保持 alpha
          ]
      ;; 加深颜色：降低亮度，略微增加饱和度
      (conj (vec darkened) a))
    color))

(defn apply-paper-texture
  "将纸纹纹理应用到颜色上（正片叠底模式）。
   paper: 纸纹图像 {:data [double] :width w :height h}
   canvas-x, canvas-y: 画布坐标
   strength: 纹理强度 0-1
   返回修改后的颜色"
  [color paper canvas-x canvas-y strength]
  (let [pw (:width paper) ph (:height paper)
        pdata (:data paper)
        ;; 平铺纹理，根据坐标采样
        tx (mod canvas-x pw)
        ty (mod canvas-y ph)
        tex-alpha (if (and (>= tx 0) (< tx pw) (>= ty 0) (< ty ph))
                    (aget pdata (+ tx (* ty pw)))
                    0.5) ;; 默认中性灰
        factor (- 1.0 (* strength tex-alpha)) ;; 0=全黑，1=无变化
        ]
    (mapv #(util/clamp 0.0 1.0 (* % factor)) color)))