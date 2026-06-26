(ns top.kzre.krro.brush.smoother
  "平滑器实现：高斯平滑、自适应 Kalman 滤波器、Cable Stabilizer (EMA 拉线)。
   输入事件为普通 map，至少包含 :x :y :timestamp。"
  (:require [top.kzre.krro.brush.util :as util]))

(defn- velocity-between [p1 p2]
  (let [dx (- (:x p2) (:x p1))
        dy (- (:y p2) (:y p1))
        dt (- (:timestamp p2) (:timestamp p1))
        dt (max dt 1.0)
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (/ dist dt)))

(defn- spacing-resample
  [pts spacing diameter]
  (let [step (* spacing diameter)]
    (loop [remaining pts
           result   []
           last-pos nil]
      (if (empty? remaining)
        result
        (let [pt  (first remaining)
              pos (:position pt)]
          (if (or (nil? last-pos) (>= (util/distance last-pos pos) step))
            (recur (rest remaining) (conj result pt) pos)
            (recur (rest remaining) result last-pos)))))))

(defn- gaussian-smooth
  [pts window-size]
  (let [half (quot window-size 2)
        n (count pts)]
    (map-indexed
      (fn [i pt]
        (let [start (max 0 (- i half))
              end   (min n (+ i half 1))
              window (subvec (vec pts) start end)
              avg-pos (util/avg-point (map :position pts))]
          (assoc pt :position avg-pos)))
      pts)))

(defn- kalman-smooth-points
  [pts process-noise measurement-noise & {:keys [velocity-adaptive?] :or {velocity-adaptive? false}}]
  (let [init-x {:estimate (double (:x (first pts))) :error 1.0}
        init-y {:estimate (double (:y (first pts))) :error 1.0}
        velocities (mapv #(velocity-between (nth pts %1) (nth pts %2)) (range (dec (count pts))) (range 1 (count pts)))
        avg-vel (if (pos? (count velocities))
                  (/ (apply + velocities) (count velocities))
                  0.1)
        smooth-1d (fn [init-state values]
                    (rest
                      (reduce
                        (fn [acc val]
                          (let [{:keys [estimate error]} (peek acc)
                                predicted estimate
                                actual-process-noise
                                (if velocity-adaptive?
                                  (let [vel (nth (cons avg-vel velocities) (dec (count acc)) avg-vel)]
                                    (+ process-noise (* process-noise (* vel 10.0))))
                                  process-noise)
                                predicted-error (+ error actual-process-noise)
                                gain (/ predicted-error (+ predicted-error measurement-noise))
                                new-estimate (+ predicted (* gain (- val predicted)))
                                new-error (* (- 1 gain) predicted-error)]
                            (conj acc {:estimate new-estimate :error new-error})))
                        [{:estimate (first values) :error 1.0}]
                        values)))
        xs (map :x pts)
        ys (map :y pts)
        smooth-xs (smooth-1d init-x xs)
        smooth-ys (smooth-1d init-y ys)]
    (mapv (fn [pt sx sy] (assoc pt :x (:estimate sx) :y (:estimate sy)))
          pts smooth-xs smooth-ys)))

(defn- cable-smooth
  [pts strength]
  (let [alpha (max 0.01 (min 1.0 strength))]
    (rest
      (reduce
        (fn [acc pt]
          (let [prev (peek acc)
                x (:x pt) y (:y pt)
                new-x (+ (:x prev) (* alpha (- x (:x prev))))
                new-y (+ (:y prev) (* alpha (- y (:y prev))))]
            (conj acc (assoc pt :x new-x :y new-y))))
        [(first pts)]
        pts))))

(defn- apply-taper
  [dabs taper-start taper-end]
  (let [n (count dabs)
        start-len (int (* n taper-start))
        end-len (int (* n taper-end))]
    (map-indexed
      (fn [i dab]
        (let [t (cond
                  (< i start-len) (/ (double i) (max 1 start-len))
                  (>= i (- n end-len)) (/ (double (- n 1 i)) (max 1 end-len))
                  :else 1.0)]
          (assoc dab :taper t)))
      dabs)))

(defmulti smooth
          "根据 stroke-spec 中的 :stabilizer 选择平滑算法。
           返回平滑并重采样后的 dab 基础参数序列。"
          (fn [input-events stroke-spec] (get stroke-spec :stabilizer :gaussian)))

(defmethod smooth :gaussian [input-events stroke-spec]
  (let [pts (vec input-events)
        window (int (get stroke-spec :smoothing 3))
        diameter (* 2.0 (get-in stroke-spec [:dab :radius] 10.0))
        spacing (get stroke-spec :spacing 0.1)
        taper-start (get stroke-spec :taper-start 0.0)
        taper-end (get stroke-spec :taper-end 0.0)
        smoothed (gaussian-smooth pts window)
        resampled (spacing-resample smoothed spacing diameter)]
    (if (or (pos? taper-start) (pos? taper-end))
      (apply-taper resampled taper-start taper-end)
      resampled)))

(defmethod smooth :kalman [input-events stroke-spec]
  (let [pts (vec input-events)
        proc-noise (get stroke-spec :smoothing 0.01)
        meas-noise (get stroke-spec :measurement-noise 0.1)
        velocity-adaptive? (get stroke-spec :velocity-adaptive? false)
        diameter (* 2.0 (get-in stroke-spec [:dab :radius] 10.0))
        spacing (get stroke-spec :spacing 0.1)
        taper-start (get stroke-spec :taper-start 0.0)
        taper-end (get stroke-spec :taper-end 0.0)
        smoothed (kalman-smooth-points pts proc-noise meas-noise :velocity-adaptive? velocity-adaptive?)
        resampled (spacing-resample smoothed spacing diameter)]
    (if (or (pos? taper-start) (pos? taper-end))
      (apply-taper resampled taper-start taper-end)
      resampled)))

(defmethod smooth :cable [input-events stroke-spec]
  (let [pts (vec input-events)
        strength (get stroke-spec :smoothing 0.3)
        diameter (* 2.0 (get-in stroke-spec [:dab :radius] 10.0))
        spacing (get stroke-spec :spacing 0.1)
        taper-start (get stroke-spec :taper-start 0.0)
        taper-end (get stroke-spec :taper-end 0.0)
        smoothed (cable-smooth pts strength)
        resampled (spacing-resample smoothed spacing diameter)]
    (if (or (pos? taper-start) (pos? taper-end))
      (apply-taper resampled taper-start taper-end)
      resampled)))

(defmethod smooth :default [input-events stroke-spec]
  (smooth input-events (assoc stroke-spec :stabilizer :gaussian)))