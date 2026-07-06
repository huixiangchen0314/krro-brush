(ns top.kzre.krro.brush.rect
  "脏矩形工具函数 —— 所有矩形均为 [x y width height] 向量。")

(defn make-rect
  "创建矩形向量，默认 [0 0 0 0]。"
  ([]
   [0 0 0 0])
  ([x y w h]
   [x y w h]))

(defn aabb->rect
  "将轴对齐包围盒转换为矩形格式。
   参数为 min-x, min-y, max-x, max-y（均为整数或浮点数）。
   返回 [min-x min-y width height]。"
  [min-x min-y max-x max-y]
  (make-rect min-x min-y (- max-x min-x) (- max-y min-y)))

(defn rect-union
  "返回包围两个矩形的最小矩形。"
  [[x1 y1 w1 h1] [x2 y2 w2 h2]]
  (if (and (zero? w1) (zero? h1))
    [x2 y2 w2 h2]
    (if (and (zero? w2) (zero? h2))
      [x1 y1 w1 h1]
      (let [ux (min x1 x2)
            uy (min y1 y2)
            uw (- (max (+ x1 w1) (+ x2 w2)) ux)
            uh (- (max (+ y1 h1) (+ y2 h2)) uy)]
        [ux uy uw uh]))))

(defn merge-rects
  "合并一系列矩形为最小包围盒。接受可变数量的矩形参数。"
  [& rects]
  (if (empty? rects)
    (make-rect)   ;; 返回 [0 0 0 0]
    (reduce rect-union (make-rect) rects)))