(ns top.kzre.krro.brush.core
  "笔刷引擎核心：五层管线，内置混色模型与后处理。
   所有组件函数均可通过参数注入替换。"
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
  "将 dab 遮罩逐像素混合到画布上，并应用后处理。"
  [canvas dab-mask fg-color opacity mix-mode mixer post-spec paper-texture]
  (let [w       (:width dab-mask)
        h       (:height dab-mask)
        data    (:data dab-mask)
        half-w  (quot w 2)
        half-h  (quot h 2)
        canvas-w (p/width canvas)
        canvas-h (p/height canvas)
        cx      (:cx dab-mask)
        cy      (:cy dab-mask)
        water-edge? (get-in post-spec [:watercolor :enable] false)
        edge-intensity (get-in post-spec [:watercolor :intensity] 0.5)
        paper-enabled? (and paper-texture (get-in post-spec [:paper :enable] false))
        paper-strength (get-in post-spec [:paper :strength] 0.2)]
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
                      mixed (mixer fg-color bg (* opacity alpha) mix-mode)
                      ;; 后处理
                      final (cond-> mixed
                                    water-edge?
                                    (post/apply-watercolor-edge dab-mask px py alpha edge-intensity)
                                    paper-enabled?
                                    (post/apply-paper-texture paper-texture canvas-x canvas-y paper-strength))]
                  (p/set-pixel! canvas-state canvas-x canvas-y final))
                canvas-state))
            canvas-state)))
      canvas
      (range (* w h)))))

(defn render-stroke
  "笔触渲染主入口。
   内置 :colored-brush 和 :smudge 混色模型。
   可选参数注入自定义实现：
     :smoother-impl   - 平滑函数 (fn [input-events stroke-spec] -> dab-bases)
     :dynamics-impl   - 动力学映射函数 (fn [dab-base dynamics-spec input-event] -> params)
     :dab-impl        - 笔尖生成函数 (fn [dab-spec params] -> dab-mask)
     :mixer-impl      - 颜色混合器，实现 IColorMixer 协议"
  [brush-def canvas input-events
   & {:keys [smoother-impl dynamics-impl dab-impl mixer-impl]
      :or {smoother-impl smoother/smooth
           dynamics-impl dynamics/map-dynamics
           dab-impl      dab/generate-dab
           mixer-impl    mix/default-mix-colors}}]
  (let [stroke-spec (:stroke brush-def)
        dyn-spec    (:dynamics brush-def)
        dab-spec    (:dab brush-def)
        color-spec  (:color brush-def)
        post-spec   (:post brush-def)
        paper-tex   (get-in brush-def [:texture :paper-image])
        mix-mode    (:blend-model color-spec :basic)
        base-fg     (get color-spec :color [0 0 0 1])
        dab-bases   (smoother-impl input-events stroke-spec)
        initial-st  (stroke/init-state)]
    (first
      (reduce
        (fn [[canvas-state st] dab-base]
          (let [event   (or (some #(when (= (:timestamp %) (:timestamp dab-base)) %) input-events)
                            (first input-events))
                params  (dynamics-impl dab-base dyn-spec event)
                taper   (:taper dab-base 1.0)
                opacity (* (:opacity params 1.0) taper)
                cx      (:x params)
                cy      (:y params)
                dab-mask (dab-impl dab-spec params)
                [fg new-st]
                (case mix-mode
                  :colored-brush
                  (let [blend-ratio (get params :blend-ratio 0.5)
                        carry-decay (get params :carry-decay 0.5)
                        carry (get st :carry-color (subvec base-fg 0 3))
                        bg (p/get-pixel canvas-state cx cy)
                        fg-rgb (subvec base-fg 0 3)
                        bg-rgb (subvec bg 0 3)
                        blended (util/lerp fg-rgb bg-rgb blend-ratio)
                        final-rgb (util/lerp carry blended carry-decay)
                        final-a   (+ (peek base-fg) (* (peek bg) (- 1 (peek base-fg))))
                        final-color (conj (vec final-rgb) final-a)
                        new-st (stroke/update-state st {:carry-color final-rgb :last-pos [cx cy]})]
                    [final-color new-st])
                  :smudge
                  (let [last-pos (:last-pos st)
                        smudge-src (if last-pos
                                     (p/get-pixel canvas-state (first last-pos) (second last-pos))
                                     (p/get-pixel canvas-state cx cy))
                        smudge-ratio (get params :smudge-ratio 0.5)
                        fg-rgb (subvec base-fg 0 3)
                        src-rgb (subvec smudge-src 0 3)
                        blended (util/lerp fg-rgb src-rgb smudge-ratio)
                        final-color (conj (vec blended) (peek base-fg))
                        new-st (stroke/update-state st {:last-pos [cx cy]})]
                    [final-color new-st])
                  [base-fg (stroke/update-state st {:last-pos [cx cy]})])
                dab-full (assoc dab-mask :cx cx :cy cy)
                new-canvas (blit-dab canvas-state dab-full fg opacity mix-mode mixer-impl post-spec paper-tex)]
            [new-canvas new-st]))
        [canvas initial-st]
        dab-bases))))