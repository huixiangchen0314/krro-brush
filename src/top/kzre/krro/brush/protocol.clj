(ns top.kzre.krro.brush.protocol
  "Krrō-Brush 核心协议：隔离运行时依赖与性能瓶颈。
   ICanvas          – 隔离宿主 UI 框架
   IColorMixer      – 隔离颜色引擎（如 krro-color）
   IDabRenderer     – 隔离 dab 混合（可替换为 SIMD/GPU 批量实现）
   IVectorRasterizer – 隔离矢量光栅化（可替换为 GPU 加速）")

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

;; 计算性能瓶颈 ======================

(defprotocol IDabRenderer
  "将 dab 遮罩混合到画布上。应用层可提供批量/GPU 实现以提升性能。"
  (render-dab [this canvas dab-mask fg-color opacity mix-mode mixer post-spec paper-texture]
    "返回 {:canvas new-canvas, :dirty-rect [x y w h]}"))

(defprotocol IVectorRasterizer
  "将贝塞尔段列表光栅化为灰度遮罩。应用层可提供 GPU 加速实现。"
  (rasterize-vector [this segments width-fn dab-size]
    "返回 {:data [double] :width w :height h} 的 dab 遮罩"))