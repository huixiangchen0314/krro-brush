(ns top.kzre.krro.brush.measure
  "笔触长度计算模块。
   为事件序列中的每个点附加累积欧氏距离（:length）。
   若事件为空则返回 []。")

(defn measure
  "返回一个新的向量，其中每个事件都附加了 :length 键（累积距离，像素）。
   输入 events 中的每个元素应包含 :x 和 :y 数值键。
   首点的 :length 为 0.0。"
  [events]
  (if (empty? events)
    []
    (let [first-pt (first events)
          start-x  (double (:x first-pt))
          start-y  (double (:y first-pt))]
      (loop [remaining (rest events)
             result    (transient [(assoc first-pt :length 0.0)])
             last-x    start-x
             last-y    start-y
             total     0.0]
        (if (empty? remaining)
          (persistent! result)
          (let [pt   (first remaining)
                x    (double (:x pt))
                y    (double (:y pt))
                dx   (- x last-x)
                dy   (- y last-y)
                dist (Math/sqrt (+ (* dx dx) (* dy dy)))
                new-total (+ total dist)]
            (recur (rest remaining)
                   (conj! result (assoc pt :length new-total))
                   x
                   y
                   new-total)))))))