(ns top.kzre.krro.brush.post
  "后处理器：对颜色混合后的局部图像进行艺术效果处理。
   当前为空操作，未来可添加水彩边界、纸纹合成等。")

(defn apply-post
  "不做任何后处理，直接返回原图。
   dab-image 为 {:data [double] :width w :height h} 的 map，
   post-spec 为笔刷定义的 :post 字段。
   返回处理后的图像，格式与输入相同。"
  [dab-image post-spec]
  dab-image)