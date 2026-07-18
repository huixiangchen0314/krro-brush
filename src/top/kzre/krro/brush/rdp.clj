(ns top.kzre.krro.brush.rdp
  "Ramer–Douglas–Peucker 事件降采样算法。
   用于减少笔刷事件点的数量，在保持形状的前提下提升渲染性能。
   包含最小距离预过滤、首尾点保护和 RDP 简化。")

(defn- filter-close-points [points min-dist]
  (reduce (fn [acc pt]
            (if-let [last (peek acc)]
              (let [dx (- (:x pt) (:x last))
                    dy (- (:y pt) (:y last))
                    d  (Math/sqrt (+ (* dx dx) (* dy dy)))]
                (if (> d min-dist) (conj acc pt) acc))
              (conj acc pt)))
          []
          points))

(defn- perpendicular-distance
  "计算点 [x y] 到线段 (x1,y1)-(x2,y2) 的垂直距离。"
  [x y x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        mag (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (if (zero? mag)
      (Math/sqrt (+ (Math/pow (- x x1) 2) (Math/pow (- y y1) 2)))
      (Math/abs (/ (+ (* dy x) (- (* dx y)) (- (* dy x1)) (* dx y1)) mag)))))

(defn- rdp
  "RDP 递归简化。points 为向量 [{:x :y ...}]，保留首尾点。"
  [points epsilon]
  (if (< (count points) 3)
    points
    (let [first-pt (first points)
          last-pt  (last points)]
      (loop [i 1, dist-max 0.0, idx-max 0]
        (if (< i (dec (count points)))
          (let [pt (nth points i)
                d  (perpendicular-distance (:x pt) (:y pt)
                                           (:x first-pt) (:y first-pt)
                                           (:x last-pt) (:y last-pt))]
            (if (> d dist-max)
              (recur (inc i) d i)
              (recur (inc i) dist-max idx-max)))
          (if (> dist-max epsilon)
            (let [left  (rdp (subvec points 0 (inc idx-max)) epsilon)
                  right (rdp (subvec points idx-max) epsilon)]
              (into (pop left) right))
            [first-pt last-pt]))))))

(defn- rdp-segment [points epsilon preserve-head preserve-tail]
  ;; 确保至少保留首尾指定数量的点，对中间部分应用 RDP
  (let [head   (take preserve-head points)
        tail   (take-last preserve-tail points)
        middle (drop preserve-head (drop-last preserve-tail points))]
    (into (vec head) (into (rdp (vec middle) epsilon) tail))))

(defn simplify
  "对事件序列进行简化：先通过最小距离过滤，再对中间部分应用 RDP 降采样，
   强制保留首尾 preserve-head 和 preserve-tail 个点。
   events       : 事件序列 [{:x :y ...}]
   epsilon      : RDP 距离阈值（像素），值越小保留的点越多
   min-dist     : 最小点间距（像素），小于此距离的点将被合并（可选，默认为 nil）
   preserve-head: 头部强制保留的点数（可选，默认 3）
   preserve-tail: 尾部强制保留的点数（可选，默认 3）"
  [events epsilon & {:keys [min-dist preserve-head preserve-tail]
                     :or {preserve-head 3 preserve-tail 3}}]
  (let [filtered (if min-dist (filter-close-points events min-dist) events)]
    (if (or (empty? filtered) (not (pos? epsilon)))
      filtered
      (rdp-segment (vec filtered) epsilon preserve-head preserve-tail))))