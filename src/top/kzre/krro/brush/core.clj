(ns top.kzre.krro.brush.core
  "笔刷引擎核心：组装五层管线，执行笔触渲染。
   内置 :colored-brush 混色模型（SAI 风格 carry），支持压感调节混合比与衰减。
   所有组件均通过参数注入，默认使用内置实现。"
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
  "将 dab 遮罩逐像素混合到画布上。"
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
   内置 :colored-brush 混色模型支持。
   可选参数注入自定义实现。"
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
        mix-mode    (:blend-model color-spec :basic)
        base-fg     (get color-spec :color [0 0 0 1])
        ;; 1. 平滑
        dab-bases   (smoother-impl input-events stroke-spec)
        initial-st  (stroke/init-state)]
    (first
      (reduce
        (fn [[canvas-state st] dab-base]
          (let [event   (or (some #(when (= (:timestamp %) (:timestamp dab-base)) %) input-events)
                            (first input-events))
                ;; 2. 动力学映射
                params  (dynamics-impl dab-base dyn-spec event)
                opacity (:opacity params 1.0)
                cx      (:x params)
                cy      (:y params)
                ;; 3. 笔尖生成
                dab-mask (dab-impl dab-spec params)
                ;; 4. 根据混色模型计算最终前景色
                [fg new-st]
                (if (= mix-mode :colored-brush)
                  (let [;; 从状态取 carry-color，若无则用基础前景色
                        carry  (get st :carry-color (subvec base-fg 0 3))
                        ;; 采样画布中心点背景色
                        bg     (p/get-pixel canvas-state cx cy)
                        ;; 用混合器混合前景与背景（普通混合）
                        blended (mixer-impl base-fg bg opacity mix-mode)
                        blended-rgb (subvec blended 0 3)
                        ;; 与携带颜色混合
                        decay  (:carry-decay params 0.5)
                        final-color (conj (vec (util/lerp carry blended-rgb decay)) (peek blended))
                        new-st (stroke/update-state st {:carry-color (subvec final-color 0 3)
                                                        :last-pos [cx cy]})]
                    [final-color new-st])
                  ;; 默认：无 carry 处理，仅更新位置
                  [base-fg (stroke/update-state st {:last-pos [cx cy]})])
                ;; 5. 将 dab 混合到画布
                dab-full (assoc dab-mask :cx cx :cy cy)
                new-canvas (blit-dab canvas-state dab-full fg opacity mix-mode mixer-impl)]
            [new-canvas new-st]))
        [canvas initial-st]
        dab-bases))))