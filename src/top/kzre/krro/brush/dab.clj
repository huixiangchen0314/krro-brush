(ns top.kzre.krro.brush.dab
  "笔尖生成器：委托给 PixelShape 库生成灰度遮罩。保留缓存与双笔尖混合。
   颜色混合已切换至 color-utils (float[])。"
  (:require
    [top.kzre.krro.brush.util :as util])
  (:import
    (clojure.lang PersistentQueue)
    (top.kzre.colorutils.color RGB)
    (top.kzre.colorutils.blend Blends)
    (top.kzre.pixelshape MaskType PixelShape)))

;; 最大缓存条目数
(def ^:private max-cache-size 256)

;; 缓存状态：包含 map 和键的 FIFO 队列
(defonce ^:private dab-cache-state
         (atom {:cache {} :keys PersistentQueue/EMPTY}))

(defn- cache-get [state key]
  (get (:cache state) key))

(defn- cache-put [state key val]
  (let [keys (:keys state)
        cache (:cache state)]
    (if (>= (count keys) max-cache-size)
      (let [oldest (peek keys)
            new-keys (conj (pop keys) key)
            new-cache (dissoc cache oldest)]
        {:cache (assoc new-cache key val) :keys new-keys})
      {:cache (assoc cache key val) :keys (conj keys key)})))

;; ── 辅助：将 Clojure 关键字转为 MaskType 枚举 ────────
(defn- ->mask-type [kw]
  (case kw
    :hard     MaskType/HARD
    :soft     MaskType/SOFT
    :gaussian MaskType/GAUSSIAN
    MaskType/SOFT))

;; ── 缓存键生成（与 PixelShape 参数对应） ──────────────
(defn- cache-key
  [dab-spec params]
  (let [t (get dab-spec :type :circle)]
    (case t
      (:circle :ellipse :star :polygon
        :rectangle :diamond :trapezoid :teardrop :crescent)
      [t (get params :radius 10.0)
       (get dab-spec :mask-type :soft)
       (when (#{:ellipse :star :polygon :rectangle :diamond :trapezoid :teardrop :crescent} t)
         (get params :angle 0.0))
       (when (= t :star) [(get dab-spec :points 5) (get dab-spec :inner-ratio 0.5)])
       (when (= t :polygon) (get dab-spec :sides 6))
       (when (= t :ellipse) [(get params :radius-x (get params :radius 10.0))
                             (get params :radius-y (get params :radius 10.0))])
       (when (= t :rectangle) [(get dab-spec :half-width 10.0)
                               (get dab-spec :half-height 10.0)
                               (get dab-spec :corner-radius 0.0)])
       (when (= t :diamond) [(get dab-spec :half-width 10.0)
                             (get dab-spec :half-height 10.0)])
       (when (= t :trapezoid) [(get dab-spec :top-half-width 5.0)
                               (get dab-spec :bottom-half-width 10.0)
                               (get dab-spec :half-height 10.0)])
       (when (= t :teardrop) [(get dab-spec :tail-length 20.0)])
       (when (= t :crescent) [(get dab-spec :outer-radius 10.0)
                              (get dab-spec :inner-radius 5.0)
                              (get dab-spec :inner-offset 3.0)])]
      :image (let [img (:image-data dab-spec)]
               [t (get params :radius 10.0)
                (get params :scale-x 1.0)
                (get params :scale-y 1.0)
                (get params :angle 0.0)
                (hash img)])
      :custom [t (get params :radius 10.0) (hash (:custom-fn dab-spec))]
      :splatter [:splatter (get params :radius 10.0)
                 (get dab-spec :mask-type :soft)
                 (get dab-spec :splatter-count 20)
                 (get dab-spec :splatter-size 0.3)
                 (get dab-spec :seed (hash dab-spec))]
      :dual (let [p (get dab-spec :primary) s (get dab-spec :secondary)]
              [:dual (cache-key p params) (cache-key s params) (get dab-spec :dual-blend :multiply)])
      ;; 默认
      [t (get params :radius 10.0) (get dab-spec :mask-type :soft)])))

(defmulti generate-dab*
          (fn [brush-spec _params] (get (:dab brush-spec) :type :circle)))

(defn- cached-generate-dab*
  [brush-spec params]
  (let [key (cache-key (:dab brush-spec) params)]
    (if-let [cached (cache-get @dab-cache-state key)]
      cached
      (let [result (generate-dab* brush-spec params)]
        (swap! dab-cache-state #(if (contains? (:cache %) key) % (cache-put % key result)))
        result))))

;; ── 各形状委托给 PixelShape，返回的 float[] 直接作为 :data ─
(defmethod generate-dab* :circle [brush-spec params]
  (let [{:keys [dab]} brush-spec
        radius (float (util/param :radius params brush-spec))
        size (int (* 2 radius))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/circle size radius mt)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :ellipse [brush-spec params]
  (let [{:keys [dab]} brush-spec
        rx (float (get params :radius-x (get params :radius 10.0)))
        ry (float (get params :radius-y (get params :radius 10.0)))
        angle (float (get params :angle 0.0))
        size (int (* 2 (max rx ry)))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/ellipse size rx ry angle mt)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :polygon [brush-spec params]
  (let [{:keys [dab]} brush-spec
        radius (float (get params :radius 10.0))
        sides (int (get dab :sides 6))
        angle (float (get params :angle 0.0))
        size (int (* 2 radius))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/polygon size radius sides angle mt)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :star [brush-spec params]
  (let [{:keys [dab]} brush-spec
        radius (float (get params :radius 10.0))
        points (int (get dab :points 5))
        inner-ratio (float (get dab :inner-ratio 0.5))
        size (int (* 2 radius))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/star size radius points inner-ratio mt)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :rectangle [brush-spec params]
  (let [{:keys [dab]} brush-spec
        half-w (float (get dab :half-width 10.0))
        half-h (float (get dab :half-height 10.0))
        corner (float (get dab :corner-radius 0.0))
        angle (float (get params :angle 0.0))
        size (int (* 2 (max half-w half-h)))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/rectangle size half-w half-h corner angle mt)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :diamond [brush-spec params]
  (let [{:keys [dab]} brush-spec
        half-w (float (get dab :half-width 10.0))
        half-h (float (get dab :half-height 10.0))
        angle (float (get params :angle 0.0))
        size (int (* 2 (max half-w half-h)))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/diamond size half-w half-h angle mt)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :trapezoid [brush-spec params]
  (let [{:keys [dab]} brush-spec
        top (float (get dab :top-half-width 5.0))
        bottom (float (get dab :bottom-half-width 10.0))
        half-h (float (get dab :half-height 10.0))
        angle (float (get params :angle 0.0))
        size (int (* 2 (max top bottom half-h)))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/trapezoid size top bottom half-h angle mt)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :teardrop [brush-spec params]
  (let [{:keys [dab]} brush-spec
        radius (float (get params :radius 10.0))
        tail (float (get dab :tail-length 20.0))
        angle (float (get params :angle 0.0))
        size (int (* 2 (max radius tail)))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/teardrop size radius tail angle mt)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :crescent [brush-spec params]
  (let [{:keys [dab]} brush-spec
        outer (float (get dab :outer-radius 10.0))
        inner (float (get dab :inner-radius 5.0))
        offset (float (get dab :inner-offset 3.0))
        angle (float (get params :angle 0.0))
        size (int (* 2 (max outer (+ inner offset))))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/crescent size outer inner offset angle mt)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :image [brush-spec params]
  (let [{:keys [dab]} brush-spec
        radius (float (get params :radius 10.0))
        img-data (get dab :image-data)
        scale-x (float (get params :scale-x 1.0))
        scale-y (float (get params :scale-y 1.0))
        angle (float (get params :angle 0.0))
        size (int (* 2 radius))
        arr (if img-data
              (PixelShape/fromImage size (float-array (:data img-data)) (:width img-data) (:height img-data)
                                    scale-x scale-y angle)
              (PixelShape/circle size radius (->mask-type (get dab :mask-type :soft))))]
    {:data arr :width size :height size}))

(defmethod generate-dab* :splatter [brush-spec params]
  (let [{:keys [dab]} brush-spec
        radius (float (get params :radius 10.0))
        count (int (get dab :splatter-count 20))
        spot-size (float (get dab :splatter-size 0.3))
        seed (long (get dab :seed (hash dab)))
        size (int (* 2 radius))
        mt (->mask-type (get dab :mask-type :soft))
        arr (PixelShape/splatter size radius count spot-size mt seed)]
    {:data arr :width size :height size}))

(defmethod generate-dab* :texture-stamp [brush-spec params]
  (let [{:keys [dab]} brush-spec
        radius (float (get params :radius 10.0))
        tex-data (get dab :texture-data)
        size (int (* 2 radius))
        mt (->mask-type (get dab :mask-type :soft))]
    (if tex-data
      (let [tex-arr (PixelShape/fromImage size (float-array (:data tex-data)) (:width tex-data) (:height tex-data) 1.0 1.0 0.0)
            mask-arr (PixelShape/circle size radius (->mask-type mt))
            n (count tex-arr)
            result (float-array n)]
        (dotimes [i n]
          (aset result i (float (* (aget tex-arr i) (aget mask-arr i)))))
        {:data result :width size :height size})
      (generate-dab* (assoc brush-spec :dab (assoc dab :type :circle)) params))))

(defmethod generate-dab* :dual [brush-spec params]
  (let [{:keys [dab]} brush-spec
        primary-spec (:primary dab)
        secondary-spec (:secondary dab)
        blend-mode (util/blend-mode-str dab Blends/MULTIPLY)
        primary (cached-generate-dab* (assoc brush-spec :dab primary-spec) params)
        secondary (cached-generate-dab* (assoc brush-spec :dab secondary-spec) params)
        size (:width primary)
        data (float-array (* size size))]
    (dotimes [i (* size size)]
      (let [a1 (float (aget (:data primary) i))
            a2 (float (aget (:data secondary) i))
            c1 (RGB/rgb a1 a1 a1)
            c2 (RGB/rgb a2 a2 a2)
            blended (Blends/blend blend-mode c1 c2)
            a (aget blended 0)]  ;; 红通道，因灰度三通道相等
        (aset data i a)))
    {:data data :width size :height size}))

(defmethod generate-dab* :custom [brush-spec params]
  (let [{:keys [dab]} brush-spec
        f (:custom-fn dab)]
    (if f
      (let [size (int (* 2 (float (get params :radius 10.0))))
            center (/ size 2.0)
            data (float-array (* size size))]
        (doseq [y (range size) x (range size)]
          (let [nx (/ (- x center) center)
                ny (/ (- y center) center)
                alpha (f nx ny)]
            (aset data (+ x (* y size)) (util/clamp01 (float alpha)))))
        {:data data :width size :height size})
      (generate-dab* (assoc brush-spec :dab {:type :circle, :mask-type :soft}) params))))

(defmethod generate-dab* :default [brush-spec params]
  (throw (ex-info "Unknown dab type." {:brush brush-spec :params params})))

(defn generate-dab
  "生成灰度 dab 遮罩（带缓存）。返回 {:data float-array, :width w, :height h}"
  [brush-spec params]
  (cached-generate-dab* brush-spec params))