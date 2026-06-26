(ns top.kzre.krro.brush.protocol
  "Krrō-Brush 核心协议：只定义行为与依赖隔离，数据保持纯 map。")

;; ── 行为抽象 ────────────────────────────────────────────

(defprotocol ISmoother
  "对原始输入点进行防抖和平滑，生成 dab 中心序列。"
  (smooth [this input-events stroke-spec]
    "接受原始输入事件序列和笔触规格（:stabilizer, :smoothing, :spacing），
     返回平滑后的 dab 基础参数序列，每个元素至少包含 :x, :y, :pressure, :velocity。"))

(defprotocol IDynamicsMapper
  "将传感器值映射为 dab 参数。"
  (map-dynamics [this dab-base dynamics-spec input-event]
    "dab-base 是平滑后的基础参数，dynamics-spec 是笔刷的 :dynamics map，
     input-event 是原始事件（用于获取传感器值）。
     返回更新后的 dab 参数（包含 :radius, :opacity, :angle 等）。"))

(defprotocol ICanvas
  (get-pixel [this x y] "返回 [r g b a]")
  (set-pixel! [this x y color] "设置像素颜色，返回新的画布状态（不可变）")
  (width [this])
  (height [this]))

(defprotocol IDabGenerator
  "生成笔尖灰度遮罩。"
  (generate-dab [this dab-spec params]
    "dab-spec 是笔刷的 :dab map，params 是动力学映射后的参数。
     返回一个灰度遮罩（如 double[] 数组），元素为 0-1 的透明度值。"))

(defprotocol IColorMixer
  "颜色混合，由宿主注入具体实现（如 krro-color 桥接）。"
  (mix-colors [this fg bg opacity blend-mode]
    "将前景色 fg 与背景色 bg 按 opacity 和 blend-mode 混合。
     fg, bg 为 [r g b a]，返回值也是 [r g b a]。")
  (mix-pigments [this pigment-keys ratios]
    "按比例混合多种颜料，返回 [r g b] 或 [r g b a]。可选实现，用于物理混色。"))

(defprotocol IPostProcessor
  "对混合后的局部图像进行后处理。"
  (apply-post [this dab-image post-spec]
    "dab-image 是颜色混合后的局部像素块，post-spec 是笔刷的 :post-process map。
     返回处理后的像素块。"))

;; ── 隔离（矢量笔刷） ────────────────────────────────────

(defprotocol IVectorFitter
  "将点序列拟合为矢量轮廓。"
  (fit-curve [this points]
    "输入带压力的点序列，返回矢量笔触数据（贝塞尔段集合和线宽数组）。"))

(defprotocol IVectorRenderer
  "将矢量轮廓光栅化为灰度遮罩。"
  (render-vector [this vector-data width-fn]
    "width-fn 是 (fn [t] width) 的函数，t 为曲线参数 0-1。
     返回灰度遮罩，供颜色混合层使用。"))