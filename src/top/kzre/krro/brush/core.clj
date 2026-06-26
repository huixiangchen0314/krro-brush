(ns top.kzre.krro.brush.core
  "笔刷引擎核心：五层管线，内置 :colored-brush 与 :smudge 混色模型。
   所有组件可注入替换。"
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
   内置混色模型：
     :basic           - 简单 alpha 混合（使用 mixer-impl）
     :colored-brush   - 携带颜色混合（SAI 风格），解耦 blend-ratio 与 carry-decay
     :smudge          - 涂抹混色（Krita 风格），采样上一位置画布色参与混合
   其他模型可通过注入 mixer-impl 实现。"
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
                ;; 4. 混色计算，返回 [最终前景色, 新笔触状态]
                [fg new-st]
                (case mix-mode
                  :colored-brush
                  (let [;; 前景与背景混合比 (0=全背景,1=全前景)
                        blend-ratio (get params :blend-ratio 0.5)
                        ;; 携带衰减 (0=立刻被背景同化,1=完全保持携带色)
                        carry-decay (get params :carry-decay 0.5)
                        ;; 从状态取携带色，若无则用基础前景色
                        carry (get st :carry-color (subvec base-fg 0 3))
                        ;; 采样画布中心点背景色
                        bg (p/get-pixel canvas-state cx cy)
                        ;; 先混合前景与背景
                        fg-rgb (subvec base-fg 0 3)
                        bg-rgb (subvec bg 0 3)
                        blended (util/lerp fg-rgb bg-rgb blend-ratio)
                        ;; 再与携带色混合
                        final-rgb (util/lerp carry blended carry-decay)
                        final-a   (+ (peek base-fg) (* (peek bg) (- 1 (peek base-fg)))) ; 简化 alpha
                        final-color (conj (vec final-rgb) final-a)
                        new-st (stroke/update-state st {:carry-color final-rgb
                                                        :last-pos [cx cy]})]
                    [final-color new-st])

                  :smudge
                  (let [;; 从状态获取上一位置
                        last-pos (:last-pos st)
                        ;; 采样上一位置的画布颜色（若无则用当前背景）
                        smudge-src (if last-pos
                                     (p/get-pixel canvas-state (first last-pos) (second last-pos))
                                     (p/get-pixel canvas-state cx cy))
                        ;; 涂抹强度 (0=完全用前景色,1=完全用采样色)
                        smudge-ratio (get params :smudge-ratio 0.5)
                        ;; 混合前景与采样色
                        fg-rgb (subvec base-fg 0 3)
                        src-rgb (subvec smudge-src 0 3)
                        blended (util/lerp fg-rgb src-rgb smudge-ratio)
                        ;; 直接作为最终颜色（可再与背景混合在 blit-dab 里完成）
                        final-color (conj (vec blended) (peek base-fg))
                        new-st (stroke/update-state st {:last-pos [cx cy]})]
                    [final-color new-st])

                  ;; 默认 :basic 或其他，直接使用 mixer-impl
                  [base-fg (stroke/update-state st {:last-pos [cx cy]})])
                ;; 5. 将 dab 混合到画布
                dab-full (assoc dab-mask :cx cx :cy cy)
                new-canvas (blit-dab canvas-state dab-full fg opacity mix-mode mixer-impl)]
            [new-canvas new-st]))
        [canvas initial-st]
        dab-bases))))