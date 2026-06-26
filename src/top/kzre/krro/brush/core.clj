(ns top.kzre.krro.brush.core
  "笔刷引擎核心：组装五层管线，执行笔触渲染。
   所有组件均通过参数注入，默认使用内置实现。
   画布通过 ICanvas 协议操作，颜色混合通过 IColorMixer 协议。"
  (:require
    [top.kzre.krro.brush.protocol :as p]
    [top.kzre.krro.brush.smoother :as smoother]
    [top.kzre.krro.brush.dynamics :as dynamics]
    [top.kzre.krro.brush.dab :as dab]
    [top.kzre.krro.brush.mix :as mix]
    [top.kzre.krro.brush.post :as post]
    [top.kzre.krro.brush.stroke :as stroke]
    [top.kzre.krro.brush.util :as util]))

(defn- blit-dab
  "将 dab 遮罩逐像素混合到画布上。
   dab-mask 是 {:data [double] :width w :height h} 的 map，
   其 :cx, :cy 字段指示 dab 中心在画布上的坐标。
   使用 ICanvas 协议读取背景像素并写回混合结果。"
  [canvas dab-mask fg-color opacity mix-mode mixer]
  (let [w       (:width dab-mask)
        h       (:height dab-mask)
        data    (:data dab-mask)
        half-w  (quot w 2)
        half-h  (quot h 2)
        canvas-w (p/width canvas)
        canvas-h (p/height canvas)
        cx      (:cx dab-mask)
        cy      (:cy dab-mask)]
    (reduce
      (fn [canvas-state idx]
        (let [px (mod idx w)
              py (quot idx w)
              alpha (aget data idx)]
          (if (> alpha 0.0)
            (let [canvas-x (- cx half-w px)
                  canvas-y (- cy half-h py)]
              (if (and (>= canvas-x 0) (< canvas-x canvas-w)
                       (>= canvas-y 0) (< canvas-y canvas-h))
                (let [bg    (p/get-pixel canvas-state canvas-x canvas-y)
                      mixed (mixer fg-color bg (* opacity alpha) mix-mode)]
                  (p/set-pixel! canvas-state canvas-x canvas-y mixed))
                canvas-state))
            canvas-state)))
      canvas
      (range (* w h)))))

(defn render-stroke
  "笔触渲染主入口。
   参数：
     brush-def     – 笔刷定义 map（符合 brush.spec）
     canvas        – 实现了 ICanvas 的画布状态
     input-events  – 输入事件序列（每个事件为 map，至少包含 :x :y）
   可选参数（用于注入自定义实现）：
     :smoother-impl   – 实现 ISmoother 的函数，默认使用 smoother/smooth
     :dynamics-impl   – 实现 IDynamicsMapper 的函数，默认 dynamics/map-dynamics
     :dab-impl        – 实现 IDabGenerator 的函数，默认 dab/generate-dab
     :mixer-impl      – 实现 IColorMixer 的函数，默认 mix/default-mix-colors
     :post-impl       – 实现 IPostProcessor 的函数，默认 post/apply-post
   返回更新后的画布状态。"
  [brush-def canvas input-events
   & {:keys [smoother-impl dynamics-impl dab-impl mixer-impl post-impl]
      :or {smoother-impl smoother/smooth
           dynamics-impl dynamics/map-dynamics
           dab-impl      dab/generate-dab
           mixer-impl    mix/default-mix-colors
           post-impl     post/apply-post}}]
  (let [stroke-spec (:stroke brush-def)
        dyn-spec    (:dynamics brush-def)
        dab-spec    (:dab brush-def)
        color-spec  (:color brush-def)
        post-spec   (:post brush-def)
        ;; 1. 平滑
        dab-bases   (smoother-impl input-events stroke-spec)
        ;; 初始笔触状态
        initial-st  (stroke/init-state)]
    ;; 2. 逐个 dab 渲染，reduce 累加器为 [canvas-state stroke-state]
    (first
      (reduce
        (fn [[canvas-state st] dab-base]
          (let [;; 匹配输入事件（简化：取第一个事件，实际应基于时间戳）
                event   (some #(when (= (:timestamp %) (:timestamp dab-base)) %) input-events)
                event   (or event (first input-events))
                ;; 3. 动力学映射
                params  (dynamics-impl dab-base dyn-spec event)
                ;; 4. 笔尖生成
                dab-mask (dab-impl dab-spec params)
                ;; 提取参数
                cx      (:x params)
                cy      (:y params)
                fg      (get color-spec :color [0 0 0 1])
                mix-mode (:blend-model color-spec :basic)
                opacity (:opacity params 1.0)
                ;; 5. 颜色混合（可能使用 stroke 状态）
                ;; 注意：对于 :sai-carry 混色模型，应在混合前将 carry-color 传递给 mixer
                mixed   (mixer-impl fg
                                    (p/get-pixel canvas-state cx cy) ;; 取中心像素作为背景，但 blit-dab 内会逐像素混合
                                    opacity mix-mode)
                ;; 实际上 mixer-impl 在这里未被逐像素调用，blit-dab 内部会调用 mixer，
                ;; 因此这里只需生成遮罩并交给 blit-dab，混合在 blit-dab 中完成。
                ;; 修正：我们直接调用 blit-dab 进行整个 dab 的混合，而不是预先混合中心颜色。
                ;; 所以这里不需要单独调用 mixer-impl。
                ;; 将 dab-mask 补充中心坐标
                dab-full (assoc dab-mask :cx cx :cy cy)
                ;; 6. 将 dab 混合到画布（内部会调用 mixer-impl 逐像素）
                new-canvas (blit-dab canvas-state dab-full fg opacity mix-mode mixer-impl)
                ;; 7. 后处理（暂不逐像素后处理，blit-dab 后没有局部图像，后处理需在 dab 混合前或整体后处理）
                ;; 当前后处理留空，未来可扩展
                ;; 8. 更新笔触状态（根据混色模型）
                new-st (if (= mix-mode :sai-carry)
                         (stroke/update-state st {:carry-color mixed :last-pos [cx cy]})
                         (stroke/update-state st {:last-pos [cx cy]}))]
            [new-canvas new-st]))
        [canvas initial-st]
        dab-bases))))