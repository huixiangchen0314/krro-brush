(ns top.kzre.krro.brush.protocol
  "Krrō-Brush 核心协议：仅隔离真正的运行时依赖。
   ICanvas     – 隔离宿主 UI 框架
   IColorMixer – 隔离颜色引擎（如 krro-color）")

(defprotocol ICanvas
  (get-pixel [this x y] "返回 [r g b a]")
  (set-pixel! [this x y color] "设置像素颜色，返回新的画布状态（不可变）")
  (width [this])
  (height [this]))

(defprotocol IColorMixer
  (mix-colors [this fg bg opacity blend-mode]
    "将前景色 fg 与背景色 bg 按 opacity 和 blend-mode 混合。
     fg, bg 为 [r g b a]，返回值也是 [r g b a]。")
  (mix-pigments [this pigment-keys ratios]
    "按比例混合多种颜料，返回 [r g b] 或 [r g b a]。可选实现，用于物理混色。"))