(ns top.kzre.krro.brush.dab
  "笔尖生成器：圆形、星形、多边形、椭圆、位图笔尖、自定义形状。
   支持双笔尖叠加、蒙版类型（hard/soft/gaussian）、纹理映射。
   全部纯函数，无外部依赖。"
  (:require [top.kzre.krro.brush.util :as util]))

;; 在文件顶部添加伪随机数生成器（线性同余）
(defn- lcg-rand
  "线性同余伪随机数生成器，返回 [next-seed, rand-val]。
   seed 为 long，返回 0..1 之间的 double。"
  [^long seed]
  (let [a 1103515245
        c 12345
        m (bit-shift-left 1 31)
        next-seed (mod (+ (* a seed) c) m)]
    [next-seed (double (/ next-seed m))]))

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


(defn- apply-mask [dist radius mask-type]
  (case mask-type
    :hard (if (<= dist radius) 1.0 0.0)
    :soft (if (>= dist radius) 0.0 (- 1.0 (/ dist radius)))
    :gaussian (if (>= dist radius) 0.0
                                   (Math/exp (- 0 (* 3.0 (/ dist radius) (/ dist radius)))))
    (if (>= dist radius) 0.0 (- 1.0 (/ dist radius)))))

(defmulti generate-dab*
          "根据 dab-spec 中的 :type 生成对应的灰度 dab 遮罩。
           params 为动力学映射后的参数，必须包含 :radius。"
          (fn [dab-spec params] (get dab-spec :type :circle)))


;; ── 缓存键生成 ───────────────────────────────────────
(defn- cache-key
  "生成用于缓存查找的键，仅包含影响形状的主要参数。"
  [dab-spec params]
  (let [t (get dab-spec :type :circle)]
    (case t
      (:circle :ellipse :star :polygon)
      [t (get params :radius 10.0)
       (get dab-spec :mask-type :soft)
       (when (#{:ellipse :star :polygon} t) (get params :angle 0.0))
       (when (= t :star) [(get dab-spec :points 5) (get dab-spec :inner-ratio 0.5)])
       (when (= t :polygon) (get dab-spec :sides 6))]
      :image (let [img (:image-data dab-spec)]
               [t (get params :radius 10.0) (get params :scale-x 1.0) (get params :scale-y 1.0)
                (get params :angle 0.0) (hash img)])  ;; 用 hash 避免大图像比较
      :custom [t (get params :radius 10.0) (hash (:custom-fn dab-spec))]
      :splatter
      [:splatter (get params :radius 10.0)
       (get dab-spec :mask-type :soft)
       (get dab-spec :seed)]
      :dual (let [p (get dab-spec :primary) s (get dab-spec :secondary)]
              [:dual (cache-key p params) (cache-key s params) (get dab-spec :dual-blend :multiply)])
      [t (get params :radius 10.0) (get dab-spec :mask-type :soft)])))  ;; 默认

;; ── 缓存原子 ─────────────────────────────────────────
(defonce ^:private dab-cache (atom {}))

(defn- cached-generate-dab*
  "带缓存的 dab 生成，键不存在时调用原始函数并存储结果。"
  [dab-spec params]
  (let [key (cache-key dab-spec params)]
    (if-let [cached (get @dab-cache key)]
      cached
      (let [result (generate-dab* dab-spec params)]
        (swap! dab-cache assoc key result)
        result))))




(defmethod generate-dab* :circle [dab-spec params]
  (let [size (int (* 2 (get params :radius 10.0)))
        radius (get params :radius 10.0)
        mask-type (get dab-spec :mask-type :soft)
        center (/ size 2.0)
        data (double-array (* size size) 0.0)]
    (doseq [y (range size) x (range size)]
      (let [dx (- x center) dy (- y center)
            dist (Math/sqrt (+ (* dx dx) (* dy dy)))
            alpha (apply-mask dist radius mask-type)]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

(defmethod generate-dab* :ellipse [dab-spec params]
  (let [size (int (* 2 (get params :radius 10.0)))
        radius-x (get params :radius-x (get params :radius 10.0))
        radius-y (get params :radius-y (get params :radius 10.0))
        angle (get params :angle 0.0)
        mask-type (get dab-spec :mask-type :soft)
        center (/ size 2.0)
        data (double-array (* size size) 0.0)
        cos-t (Math/cos (- angle)) sin-t (Math/sin (- angle))]
    (doseq [y (range size) x (range size)]
      (let [dx (- x center) dy (- y center)
            rx (+ (* dx cos-t) (* dy sin-t))
            ry (- (* dy cos-t) (* dx sin-t))
            nx (/ rx radius-x) ny (/ ry radius-y)
            dist (Math/sqrt (+ (* nx nx) (* ny ny)))
            alpha (apply-mask dist 1.0 mask-type)]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

(defmethod generate-dab* :star [dab-spec params]
  (let [size (int (* 2 (get params :radius 10.0)))
        radius (get params :radius 10.0)
        points (get dab-spec :points 5)
        inner-ratio (get dab-spec :inner-ratio 0.5)
        mask-type (get dab-spec :mask-type :soft)
        center (/ size 2.0)
        data (double-array (* size size) 0.0)
        inner-radius (* radius inner-ratio)
        half-step (/ Math/PI points)]
    (doseq [y (range size) x (range size)]
      (let [dx (- x center) dy (- y center)
            dist (Math/sqrt (+ (* dx dx) (* dy dy)))
            angle (Math/atan2 dy dx)
            sector (mod angle (* 2 half-step))
            t (/ sector half-step)
            r (if (< t 1.0)
                (util/lerp radius inner-radius t)
                (util/lerp inner-radius radius (- t 1.0)))
            alpha (if (<= dist r) (apply-mask dist r mask-type) 0.0)]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

(defmethod generate-dab* :polygon [dab-spec params]
  (let [size (int (* 2 (get params :radius 10.0)))
        radius (get params :radius 10.0)
        sides (get dab-spec :sides 6)
        angle (get params :angle 0.0)
        mask-type (get dab-spec :mask-type :soft)
        center (/ size 2.0)
        data (double-array (* size size) 0.0)
        half-sector (/ Math/PI sides)]
    (doseq [y (range size) x (range size)]
      (let [dx (- x center) dy (- y center)
            dist (Math/sqrt (+ (* dx dx) (* dy dy)))
            a (- (Math/atan2 dy dx) angle)
            a (mod (+ a Math/PI Math/PI) (* 2 Math/PI))
            sector (mod a (* 2 half-sector))
            r (if (< sector half-sector)
                (/ radius (Math/cos (- sector half-sector)))
                (/ radius (Math/cos (- (* 2 half-sector) sector))))
            alpha (if (<= dist r) (apply-mask dist r mask-type) 0.0)]
        (aset-double data (+ x (* y size)) alpha)))
    {:data data :width size :height size}))

(defmethod generate-dab* :image [dab-spec params]
  (let [size (int (* 2 (get params :radius 10.0)))
        radius (get params :radius 10.0)
        scale-x (get params :scale-x 1.0)
        scale-y (get params :scale-y 1.0)
        angle (get params :angle 0.0)
        mask-type (get dab-spec :mask-type :soft)
        image-data (get dab-spec :image-data)]
    (if image-data
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
                alpha (bilinear-sample img-arr img-w img-h sx sy)]
            (aset-double data (+ x (* y size)) (if (pos? alpha) alpha 0.0))))
        {:data data :width size :height size})
      (generate-dab* {:type :circle, :mask-type mask-type} params))))

(defmethod generate-dab* :custom [dab-spec params]
  (let [size (int (* 2 (get params :radius 10.0)))
        f (get dab-spec :custom-fn)]
    (if f
      (let [center (/ size 2.0)
            data (double-array (* size size) 0.0)]
        (doseq [y (range size) x (range size)]
          (let [nx (/ (- x center) center)
                ny (/ (- y center) center)
                alpha (f nx ny)]
            (aset-double data (+ x (* y size)) (util/clamp 0.0 1.0 alpha))))
        {:data data :width size :height size})
      (generate-dab* {:type :circle} params))))

(defmethod generate-dab* :dual [dab-spec params]
  (let [primary-spec (get dab-spec :primary)
        secondary-spec (get dab-spec :secondary)
        blend-mode (get dab-spec :dual-blend :multiply)
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
                       (* a1 a2)))))
    {:data data :width size :height size}))

;; ── 喷溅笔尖 ───────────────────────────────────────
;; 修改 :splatter 方法，加入 seed 支持
(defmethod generate-dab* :splatter [dab-spec params]
  (let [size (int (* 2 (get params :radius 10.0)))
        radius (get params :radius 10.0)
        mask-type (get dab-spec :mask-type :soft)
        center (/ size 2.0)
        data (double-array (* size size) 0.0)
        splatter-count (get dab-spec :splatter-count 20)
        splatter-size (get dab-spec :splatter-size 0.3)
        ;; 获取种子：优先使用 dab-spec 中的 :seed，否则基于缓存键哈希
        seed (or (:seed dab-spec) (hash (cache-key dab-spec params)))
        _ (assert (integer? seed) "Seed must be an integer")
        ;; 第一次调用，初始化状态
        [rand-state _] (lcg-rand seed)
        ;; 生成 splatter-count 个随机点
        spots (loop [i 0 state rand-state res []]
                (if (< i splatter-count)
                  (let [[s1 r1] (lcg-rand state)
                        [s2 r2] (lcg-rand s1)
                        [s3 r3] (lcg-rand s2)
                        angle (* 2 Math/PI r1)
                        dist (* radius (Math/sqrt r2))
                        sx (+ center (* dist (Math/cos angle)))
                        sy (+ center (* dist (Math/sin angle)))
                        spot-radius (* radius splatter-size)]
                    (recur (inc i) s3 (conj res [sx sy spot-radius])))
                  res))]
    ;; 绘制每个小圆点
    (doseq [[sx sy spot-radius] spots]
      (let [min-x (max 0 (int (- sx spot-radius)))
            max-x (min (dec size) (int (+ sx spot-radius)))
            min-y (max 0 (int (- sy spot-radius)))
            max-y (min (dec size) (int (+ sy spot-radius)))]
        (doseq [y (range min-y (inc max-y))
                x (range min-x (inc max-x))]
          (let [dx (- x sx) dy (- y sy)
                dist (Math/sqrt (+ (* dx dx) (* dy dy)))
                alpha (apply-mask dist spot-radius mask-type)]
            (when (> alpha 0.0)
              (let [idx (+ x (* y size))]
                (aset-double data idx (max (aget data idx) alpha))))))))
    {:data data :width size :height size}))

;; ── 纹理印章笔尖 ────────────────────────────────────
(defmethod generate-dab* :texture-stamp [dab-spec params]
  (let [size (int (* 2 (get params :radius 10.0)))
        radius (get params :radius 10.0)
        mask-type (get dab-spec :mask-type :soft)
        texture-data (get dab-spec :texture-data)
        center (/ size 2.0)
        data (double-array (* size size) 0.0)]
    (if texture-data
      (let [tw (:width texture-data) th (:height texture-data)
            tarr (:data texture-data)
            src-cx (/ tw 2.0) src-cy (/ th 2.0)
            scale (/ radius (max src-cx src-cy))]  ;; 缩放纹理使最长边等于半径
        (doseq [y (range size)
                x (range size)]
          (let [;; 逆映射：dab 坐标 -> 纹理坐标
                dx (- x center)
                dy (- y center)
                sx (+ (* dx (/ 1.0 scale)) src-cx)
                sy (+ (* dy (/ 1.0 scale)) src-cy)
                tex-alpha (bilinear-sample tarr tw th sx sy)
                ;; 可选蒙版（软边圆形裁剪）
                dist (Math/sqrt (+ (* dx dx) (* dy dy)))
                mask (apply-mask dist radius mask-type)
                alpha (* tex-alpha mask)]
            (aset-double data (+ x (* y size)) (util/clamp 0.0 1.0 alpha)))))
      ;; 无纹理时回退为圆形
      (generate-dab* (assoc dab-spec :type :circle) params))
    {:data data :width size :height size}))

(defmethod generate-dab* :default [dab-spec params]
  (generate-dab* {:type :circle} params))

(defn generate-dab
  "生成灰度 dab 遮罩（带缓存）。返回 {:data [double], :width w, :height h}"
  [dab-spec params]
  (cached-generate-dab* dab-spec params))