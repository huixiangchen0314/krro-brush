(ns top.kzre.krro.brush.smoother
  "平滑器实现：高斯平滑 + 间距重采样，产生 dab 中心序列。
   输入事件为普通 map，至少包含 :x :y。"
  (:require [top.kzre.krro.brush.util :as util]))

(defn- gaussian-smooth
  "对点序列进行滑动窗口平均（位置分量），窗口大小 window-size 应为奇数。"
  [pts window-size]
  (let [half (quot window-size 2)
        n (count pts)]
    (map-indexed
      (fn [i pt]
        (let [start (max 0 (- i half))
              end   (min n (+ i half 1))
              window (subvec (vec pts) start end)
              avg-pos (util/avg-point (mapv :position pts))]
          (assoc pt :position avg-pos)))
      pts)))

(defn- spacing-resample
  "根据间距和直径，对已平滑的点序列进行等距重采样，确保 dab 中心均匀分布。"
  [pts spacing diameter]
  (let [step (* spacing diameter)]
    (loop [remaining pts
           result   []
           last-pos nil]
      (if (empty? remaining)
        result
        (let [pt  (first remaining)
              pos (:position pt)]
          (if (or (nil? last-pos)
                  (>= (util/distance last-pos pos) step))
            (recur (rest remaining) (conj result pt) pos)
            (recur (rest remaining) result last-pos)))))))

(defn smooth
  "平滑主函数。实现 ISmoother 协议。
   input-events: 原始事件序列（每个至少含 :x :y :pressure :velocity :timestamp）
   stroke-spec:  笔触规格（:smoothing, :spacing, :dab 子 map 含 :radius）"
  [input-events stroke-spec]
  (let [window   (int (get stroke-spec :smoothing 3))
        diameter (* 2.0 (get-in stroke-spec [:dab :radius] 10.0)) ;; 临时用笔刷定义中的默认半径
        spacing  (get stroke-spec :spacing 0.1)
        smoothed (gaussian-smooth (vec input-events) window)]
    (spacing-resample smoothed spacing diameter)))