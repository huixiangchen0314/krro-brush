(ns top.kzre.krro.brush.core
  "笔刷引擎核心：五层管线，内置混色模型与后处理，支持矢量笔刷。
   所有组件函数均可通过参数注入替换。"
  (:require
    [top.kzre.krro.brush.protocol :as p]
    [top.kzre.krro.brush.smoother :as smoother]
    [top.kzre.krro.brush.dynamics :as dynamics]
    [top.kzre.krro.brush.dab :as dab]
    [top.kzre.krro.brush.mix :as mix]
    [top.kzre.krro.brush.post :as post]
    [top.kzre.krro.brush.stroke :as stroke]
    [top.kzre.krro.brush.util :as util]
    [top.kzre.krro.brush.vector.fit :as vfit]
    [top.kzre.krro.brush.vector.render :as vrender]))

(defmulti compute-mix
          "根据混色模式 mix-mode 计算当前 dab 的最终前景色和新的笔触状态。
           返回 [fg new-st]。
           base-fg 为笔刷基础前景色 [r g b a]，
           params 为当前 dab 的动力学参数（包含 :blend-ratio, :carry-decay 等），
           st 为当前笔触状态，canvas 为画布，cx/cy 为当前 dab 中心。"
          (fn [mix-mode base-fg params st canvas cx cy] mix-mode))

(defmethod compute-mix :basic
  [_ base-fg params st canvas cx cy]
  [base-fg (stroke/update-state st {:last-pos [cx cy]})])

;; 在 core.clj 中修改 :colored-brush 方法
(defmethod compute-mix :colored-brush
  [_ base-fg params st canvas cx cy]
  (let [blend-ratio    (get params :blend-ratio 0.5)
        carry-decay    (get params :carry-decay 0.5)
        ;; 新增参数：非线性衰减指数（>1 加速衰减，<1 延迟衰减）
        decay-exponent (get params :decay-exponent 1.0)
        ;; 新增参数：最小携带量 0-1，防止颜色完全被背景覆盖
        min-carry      (get params :min-carry 0.0)
        carry (get st :carry-color (subvec base-fg 0 3))
        bg (p/get-pixel canvas cx cy)
        fg-rgb (subvec base-fg 0 3)
        bg-rgb (subvec bg 0 3)
        blended (util/lerp fg-rgb bg-rgb blend-ratio)
        ;; 应用非线性衰减
        effective-decay (Math/pow carry-decay decay-exponent)
        ;; 限制衰减后的携带色不低于 min-carry
        clamped-decay (max min-carry effective-decay)
        final-rgb (util/lerp carry blended clamped-decay)
        final-a   (+ (peek base-fg) (* (peek bg) (- 1 (peek base-fg))))
        final-color (conj (vec final-rgb) final-a)
        new-st (stroke/update-state st {:carry-color final-rgb :last-pos [cx cy]})]
    [final-color new-st]))

;; 在 core.clj 中增强 :smudge 方法
(defmethod compute-mix :smudge
  [_ base-fg params st canvas cx cy]
  (let [last-pos (:last-pos st)
        ;; 新增：采样半径（像素），默认 10.0
        smudge-radius (get params :smudge-radius 10.0)
        ;; 新增：强度 0-1，影响前景色与采样色的混合权重
        smudge-strength (get params :smudge-strength 0.5)
        ;; 采样位置（若无上一位置则用当前中心）
        sample-x (or (first last-pos) cx)
        sample-y (or (second last-pos) cy)
        ;; 简单采样：取采样点颜色（未来可改为多点平均）
        smudge-src (p/get-pixel canvas (int sample-x) (int sample-y))
        fg-rgb (subvec base-fg 0 3)
        src-rgb (subvec smudge-src 0 3)
        ;; 混合：smudge-strength 控制采样色占比（0=全前景，1=全采样）
        blended (util/lerp fg-rgb src-rgb smudge-strength)
        final-color (conj (vec blended) (peek base-fg))
        new-st (stroke/update-state st {:last-pos [cx cy]})]
    [final-color new-st]))


(defmethod compute-mix :dulling
  [_ base-fg params st canvas cx cy]
  (let [;; 钝化强度：0 = 完全用采样色，1 = 完全用前景色
        dulling-ratio (get params :dulling-ratio 0.5)
        ;; 整体不透明度
        dulling-opacity (get params :dulling-opacity 1.0)
        ;; 采样画布当前点颜色
        bg (p/get-pixel canvas cx cy)
        bg-rgb (subvec bg 0 3)
        ;; 前景色
        fg-rgb (subvec base-fg 0 3)
        ;; 混合前景与采样色
        dulled-rgb (util/lerp bg-rgb fg-rgb dulling-ratio)
        ;; 应用整体不透明度（与 base-fg alpha 结合）
        final-a (* (peek base-fg) dulling-opacity)
        final-color (conj (vec dulled-rgb) final-a)
        new-st (stroke/update-state st {:last-pos [cx cy]})]
    [final-color new-st]))


(defmethod compute-mix :default
  [mix-mode base-fg params st canvas cx cy]
  (compute-mix :basic base-fg params st canvas cx cy))


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
   支持矢量笔刷（当 brush-def 包含 :vector 字段时）。
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
  (let [vector-spec (:vector brush-def)]
    (if vector-spec
      ;; 矢量笔刷分支
      (let [base-width (get vector-spec :base-width 5.0)
            width-dynamics (get vector-spec :width-dynamics :pressure)
            dab-size (get vector-spec :dab-size 512)
            fitted (vfit/fit-curve input-events)
            width-fn (fn [t]
                       (let [seg (nth fitted (min (dec (count fitted)) (int (* t (count fitted)))))
                             pressure (:pressure seg 1.0)]
                         (* base-width pressure)))
            dab-mask (vrender/render-vector {:segments fitted} width-fn dab-size)
            color-spec (:color brush-def)
            fg (get color-spec :color [0 0 0 1])
            mix-mode (:blend-model color-spec :basic)
            opacity 1.0
            post-spec (:post brush-def)
            paper-tex (get-in brush-def [:texture :paper-image])
            xs (map :x input-events)
            ys (map :y input-events)
            minx (apply min xs) maxx (apply max xs)
            miny (apply min ys) maxy (apply max ys)
            cx (+ minx (/ (- maxx minx) 2))
            cy (+ miny (/ (- maxy miny) 2))
            dab-full (assoc dab-mask :cx cx :cy cy)]
        (blit-dab canvas dab-full fg opacity mix-mode mixer-impl post-spec paper-tex))
      ;; 光栅笔刷分支
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
                    [fg new-st] (compute-mix mix-mode base-fg params st canvas-state cx cy)
                    dab-full (assoc dab-mask :cx cx :cy cy)
                    new-canvas (blit-dab canvas-state dab-full fg opacity mix-mode mixer-impl post-spec paper-tex)]
                [new-canvas new-st]))
            [canvas initial-st]
            dab-bases))))))