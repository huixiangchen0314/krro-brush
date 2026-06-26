(ns top.kzre.krro.brush.dab
  "笔尖生成器：生成各种类型的灰度 dab 遮罩。
   支持圆形、位图笔尖（纯数据，无 AWT 依赖）、自定义函数形状，
   可应用旋转和缩放。所有图像操作使用 double 数组和双线性插值。"
  (:require [top.kzre.krro.brush.util :as util]))

;; ── 双线性采样（从灰度图像数据中取值）───────────────
(defn- bilinear-sample
  "从灰度图像 data (行主序 double 数组，宽度 w，高度 h) 中采样 (x,y) 的灰度值。
   x, y 为像素坐标（可能带小数），超出边界返回 0.0。"
  [data w h x y]
  (let [x0 (int (Math/floor x))
        y0 (int (Math/floor y))
        x1 (inc x0)
        y1 (inc y0)
        fx (- x x0)
        fy (- y y0)
        get-px (fn [px py]
                 (if (and (>= px 0) (< px w)
                          (>= py 0) (< py h))
                   (aget data (+ px (* py w)))
                   0.0))]
    (util/lerp (util/lerp (get-px x0 y0) (get-px x1 y0) fx)
               (util/lerp (get-px x0 y1) (get-px x1 y1) fx)
               fy)))

;; ── 圆形笔尖 ─────────────────────────────────────────
(defn- generate-circle-dab
  "生成圆形灰度 dab，size x size，radius 半径，hardness 边缘硬度。"
  [size radius hardness]
  (let [center (/ size 2.0)
        data   (double-array (* size size) 0.0)]
    (doseq [y (range size)
            x (range size)]
      (let [dx (- x center)
            dy (- y center)
            dist (Math/sqrt (+ (* dx dx) (* dy dy)))
            alpha (if (>= dist radius)
                    0.0
                    (if (zero? hardness)
                      (- 1.0 (/ dist radius))
                      (util/clamp 0.0 1.0
                                  (Math/pow (- 1.0 (/ dist radius)) hardness))))]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

;; ── 位图笔尖 ─────────────────────────────────────────
(defn- generate-image-dab
  "从灰度图像数据生成 dab。image-data 为 {:data [double] :width w :height h}，
   通过逆变换对 dab 每个像素采样，支持旋转 angle (度) 和缩放 scale-x, scale-y。"
  [image-data size radius scale-x scale-y angle]
  (let [center (/ size 2.0)
        data (double-array (* size size) 0.0)
        img-w (:width image-data)
        img-h (:height image-data)
        img-arr (:data image-data)
        ;; 角度转弧度
        theta (util/deg->rad angle)
        cos-t (Math/cos theta)
        sin-t (Math/sin theta)
        ;; 逆缩放
        inv-sx (/ 1.0 scale-x)
        inv-sy (/ 1.0 scale-y)
        ;; 源图像中心
        src-cx (/ img-w 2.0)
        src-cy (/ img-h 2.0)]
    (doseq [y (range size)
            x (range size)]
      (let [;; 平移到 dab 中心
            dx (- x center)
            dy (- y center)
            ;; 逆旋转
            rx (- (* dx cos-t) (* dy sin-t))
            ry (+ (* dx sin-t) (* dy cos-t))
            ;; 逆缩放并平移到源图像空间
            sx (+ (* rx inv-sx) src-cx)
            sy (+ (* ry inv-sy) src-cy)
            alpha (bilinear-sample img-arr img-w img-h sx sy)]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

;; ── 自定义函数笔尖 ──────────────────────────────────
(defn- generate-custom-dab
  "通过用户提供的函数 f 生成 dab。f 接受归一化坐标 [x y] ∈ [-1,1]，返回 alpha。"
  [size f]
  (let [center (/ size 2.0)
        data (double-array (* size size) 0.0)]
    (doseq [y (range size)
            x (range size)]
      (let [nx (/ (- x center) center)
            ny (/ (- y center) center)
            alpha (f nx ny)]
        (aset-double data (+ x (* y size)) (util/clamp 0.0 1.0 alpha))))
    {:data data :width size :height size}))

;; ── 主生成函数 ──────────────────────────────────────
(defn generate-dab
  "根据 dab-spec 和动态参数生成灰度 dab 遮罩。
   dab-spec 可包含：
     :type        - :circle, :image, :custom
     :hardness    - 默认 0.8
     :image-data  - 灰度图像 map {:data [double] :width w :height h}，用于 :image
     :custom-fn   - 自定义形状函数，用于 :custom
   params 来自动力学映射，包含：
     :radius, :hardness, :scale-x, :scale-y, :angle
   返回 {:data [double], :width w, :height h}"
  [dab-spec params]
  (let [type      (get dab-spec :type :circle)
        radius    (get params :radius 10.0)
        hardness  (get params :hardness (get dab-spec :hardness 0.8))
        scale-x   (get params :scale-x 1.0)
        scale-y   (get params :scale-y 1.0)
        angle     (get params :angle 0.0)
        size      (int (* 2 radius (max scale-x scale-y)))]
    (case type
      :circle (generate-circle-dab size radius hardness)
      :image  (if-let [img-data (get dab-spec :image-data)]
                (generate-image-dab img-data size radius scale-x scale-y angle)
                (generate-circle-dab size radius hardness))
      :custom (if-let [f (get dab-spec :custom-fn)]
                (generate-custom-dab size f)
                (generate-circle-dab size radius hardness))
      (generate-circle-dab size radius hardness))))