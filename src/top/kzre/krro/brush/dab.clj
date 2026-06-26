(ns top.kzre.krro.brush.dab
  "笔尖生成器：圆形、星形、多边形、椭圆、位图笔尖、自定义形状。
   支持双笔尖叠加、蒙版类型（hard/soft/gaussian）、纹理映射。
   全部纯函数，无外部依赖。"
  (:require [top.kzre.krro.brush.util :as util]))
(declare generate-dab*)

;; ── 辅助：双线性采样 ────────────────────────────────
(defn- bilinear-sample
  [data w h x y]
  (let [x0 (int (Math/floor x)) y0 (int (Math/floor y))
        x1 (inc x0) y1 (inc y0)
        fx (- x x0) fy (- y y0)
        get (fn [px py]
              (if (and (>= px 0) (< px w) (>= py 0) (< py h))
                (aget data (+ px (* py w))) 0.0))]
    (util/lerp (util/lerp (get x0 y0) (get x1 y0) fx)
               (util/lerp (get x0 y1) (get x1 y1) fx) fy)))

;; ── 蒙版函数 ─────────────────────────────────────────
(defn- apply-mask [dist radius mask-type]
  (case mask-type
    :hard (if (<= dist radius) 1.0 0.0)
    :soft (if (>= dist radius) 0.0 (- 1.0 (/ dist radius)))
    :gaussian (if (>= dist radius) 0.0
                                   (Math/exp (- 0 (* 3.0 (/ dist radius) (/ dist radius)))))
    ;; 默认软边
    (if (>= dist radius) 0.0 (- 1.0 (/ dist radius)))))

;; ── 圆形 ────────────────────────────────────────────
(defn- generate-circle-dab [size radius mask-type]
  (let [center (/ size 2.0)
        data (double-array (* size size) 0.0)]
    (doseq [y (range size) x (range size)]
      (let [dx (- x center) dy (- y center)
            dist (Math/sqrt (+ (* dx dx) (* dy dy)))
            alpha (apply-mask dist radius mask-type)]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

;; ── 椭圆 ────────────────────────────────────────────
(defn- generate-ellipse-dab [size radius-x radius-y angle mask-type]
  (let [center (/ size 2.0)
        data (double-array (* size size) 0.0)
        cos-t (Math/cos (- angle))
        sin-t (Math/sin (- angle))]
    (doseq [y (range size) x (range size)]
      (let [dx (- x center) dy (- y center)
            ;; 旋转回主轴
            rx (+ (* dx cos-t) (* dy sin-t))
            ry (- (* dy cos-t) (* dx sin-t))
            ;; 缩放
            nx (/ rx radius-x)
            ny (/ ry radius-y)
            dist (Math/sqrt (+ (* nx nx) (* ny ny)))
            alpha (apply-mask dist 1.0 mask-type)]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

;; ── 星形 ────────────────────────────────────────────
(defn- generate-star-dab [size radius points inner-ratio mask-type]
  (let [center (/ size 2.0)
        data (double-array (* size size) 0.0)
        inner-radius (* radius inner-ratio)
        half-step (/ Math/PI points)]
    (doseq [y (range size) x (range size)]
      (let [dx (- x center) dy (- y center)
            dist (Math/sqrt (+ (* dx dx) (* dy dy)))
            angle (Math/atan2 dy dx)
            ;; 计算该角度对应的外接半径
            sector (mod angle (* 2 half-step))
            t (/ sector half-step) ;; 0..1 within the sector
            ;; 半径在内外径之间线性插值
            r (if (< t 1.0)
                (util/lerp radius inner-radius t)
                (util/lerp inner-radius radius (- t 1.0)))
            alpha (if (<= dist r) (apply-mask dist r mask-type) 0.0)]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

;; ── 多边形 ──────────────────────────────────────────
(defn- generate-polygon-dab [size radius sides angle mask-type]
  (let [center (/ size 2.0)
        data (double-array (* size size) 0.0)
        half-sector (/ Math/PI sides)]
    (doseq [y (range size) x (range size)]
      (let [dx (- x center) dy (- y center)
            dist (Math/sqrt (+ (* dx dx) (* dy dy)))
            a (- (Math/atan2 dy dx) angle)
            ;; 归一化到 [0, 2π)
            a (mod (+ a Math/PI Math/PI) (* 2 Math/PI))
            ;; 映射到第一扇区
            sector (mod a (* 2 half-sector))
            ;; 计算该角度方向上的多边形半径
            r (if (< sector half-sector)
                (/ radius (Math/cos (- sector half-sector)))
                (/ radius (Math/cos (- (* 2 half-sector) sector))))
            alpha (if (<= dist r) (apply-mask dist r mask-type) 0.0)]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

;; ── 位图笔尖 ────────────────────────────────────────
(defn- generate-image-dab [image-data size radius scale-x scale-y angle mask-type]
  (let [center (/ size 2.0)
        data (double-array (* size size) 0.0)
        img-w (:width image-data) img-h (:height image-data)
        img-arr (:data image-data)
        theta (util/deg->rad angle)
        cos-t (Math/cos theta) sin-t (Math/sin theta)
        inv-sx (/ 1.0 scale-x) inv-sy (/ 1.0 scale-y)
        src-cx (/ img-w 2.0) src-cy (/ img-h 2.0)]
    (doseq [y (range size) x (range size)]
      (let [dx (- x center) dy (- y center)
            rx (- (* dx cos-t) (* dy sin-t))
            ry (+ (* dx sin-t) (* dy cos-t))
            sx (+ (* rx inv-sx) src-cx)
            sy (+ (* ry inv-sy) src-cy)
            alpha (bilinear-sample img-arr img-w img-h sx sy)
            ;; 应用蒙版（位图外区域自动为0）
            final-alpha (if (pos? alpha) alpha 0.0)]
        (aset-double data (+ x (* y size)) final-alpha)))
    {:data data :width size :height size}))

;; ── 自定义形状 ──────────────────────────────────────
(defn- generate-custom-dab [size f]
  (let [center (/ size 2.0)
        data (double-array (* size size) 0.0)]
    (doseq [y (range size) x (range size)]
      (let [nx (/ (- x center) center)
            ny (/ (- y center) center)
            alpha (f nx ny)]
        (aset-double data (+ x (* y size)) (util/clamp 0.0 1.0 alpha))))
    {:data data :width size :height size}))

;; ── 双笔尖叠加 ──────────────────────────────────────
(defn- generate-dual-dab [dab-spec params]
  (let [primary-spec (get dab-spec :primary)
        secondary-spec (get dab-spec :secondary)
        blend-mode (get dab-spec :dual-blend :multiply)
        ;; 生成两个笔尖
        primary (generate-dab* primary-spec params)
        secondary (generate-dab* secondary-spec params)
        size (:width primary)
        data (double-array (* size size) 0.0)]
    (dotimes [i (* size size)]
      (let [a1 (aget (:data primary) i)
            a2 (aget (:data secondary) i)]
        (aset-double data i
                     (case blend-mode
                       :multiply (* a1 a2)
                       :screen   (- (+ a1 a2) (* a1 a2))
                       :overlay  (if (< a1 0.5) (* 2 a1 a2) (- 1 (* 2 (- 1 a1) (- 1 a2))))
                       :max      (max a1 a2)
                       :min      (min a1 a2)
                       ;; 默认 multiply
                       (* a1 a2)))))
    {:data data :width size :height size}))

;; ── 内部生成函数（支持所有类型） ──────────────────────
(defn- generate-dab* [dab-spec params]
  (let [type       (get dab-spec :type :circle)
        radius     (get params :radius 10.0)
        mask-type  (get dab-spec :mask-type :soft)
        size       (int (* 2 radius))]
    (case type
      :circle   (generate-circle-dab size radius mask-type)
      :ellipse  (generate-ellipse-dab size
                                      (get params :radius-x radius)
                                      (get params :radius-y radius)
                                      (get params :angle 0.0)
                                      mask-type)
      :star     (generate-star-dab size radius
                                   (get dab-spec :points 5)
                                   (get dab-spec :inner-ratio 0.5)
                                   mask-type)
      :polygon  (generate-polygon-dab size radius
                                      (get dab-spec :sides 6)
                                      (get params :angle 0.0)
                                      mask-type)
      :image    (if-let [img-data (get dab-spec :image-data)]
                  (generate-image-dab img-data size radius
                                      (get params :scale-x 1.0)
                                      (get params :scale-y 1.0)
                                      (get params :angle 0.0)
                                      mask-type)
                  (generate-circle-dab size radius mask-type))
      :custom   (if-let [f (get dab-spec :custom-fn)]
                  (generate-custom-dab size f)
                  (generate-circle-dab size radius mask-type))
      :dual     (generate-dual-dab dab-spec params)
      ;; 默认圆形
      (generate-circle-dab size radius mask-type))))

;; ── 主生成函数 ──────────────────────────────────────
(defn generate-dab
  "根据 dab-spec 和动态参数生成灰度 dab 遮罩。
   支持类型：:circle, :ellipse, :star, :polygon, :image, :custom, :dual
   支持蒙版：:hard, :soft, :gaussian (在 dab-spec 中设置 :mask-type)
   返回 {:data [double], :width w, :height h}"
  [dab-spec params]
  (generate-dab* dab-spec params))