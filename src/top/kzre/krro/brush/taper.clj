(ns top.kzre.krro.brush.taper
  "锥化处理：对笔触的半径和透明度施加起笔/收笔渐隐效果。
   支持不同字段使用不同的锥化长度或比例。"
  (:require [top.kzre.krro.brush.util :as util]))

;; ── 累积长度计算 ──────────────────────────────────
(defn- cumulative-lengths [params]
  (if (<= (count params) 1)
    {:lengths [0.0] :total 0.0}
    (let [points   (mapv #(vector (:x %) (:y %)) params)
          pairs    (partition 2 1 points)
          seg-lens (map #(util/distance (first %) (second %)) pairs)
          cumulative (reductions + 0.0 seg-lens)]
      {:lengths (vec cumulative)
       :total   (last cumulative)})))

;; ── 锥化规格 → 像素长度 ───────────────────────────
(defn- taper-spec->pixels [taper-spec total-length]
  (cond
    (number? taper-spec)
    (if (<= 0.0 taper-spec 1.0)          ;; [0,1] → 比例
      (* taper-spec total-length)
      taper-spec)                        ;; >1 → 像素

    (map? taper-spec)
    (let [{:keys [value mode]} taper-spec]
      (case mode
        :length value
        :ratio  (* value total-length)
        ;; 未指定 mode 时的推断
        (if (<= 0.0 value 1.0)
          (* value total-length)
          value)))

    :else 0.0))

;; ── 字段 → 锥化长度解析 ───────────────────────────
(defn- resolve-field-tapers [fields tapering-spec total-length]
  (cond
    (nil? tapering-spec) {}
    (map? tapering-spec)
    ;; 若 fields 提供，则只处理这些字段；否则使用 tapering-spec 的所有键
    (let [keys (or fields (vec (keys tapering-spec)))]
      (into {}
            (map (fn [f]
                   (if-let [spec (get tapering-spec f)]
                     [f (taper-spec->pixels spec total-length)]
                     [f 0.0]))
                 keys)))
    :else
    ;; 单值：所有 fields 共用同一个长度
    (let [len (taper-spec->pixels tapering-spec total-length)]
      (into {} (map #(vector % len) fields)))))

;; ── 锥化因子计算 ──────────────────────────────────
(defn- taper-factor [dist total start-len end-len]
  (let [start-factor (if (pos? start-len)
                       (min 1.0 (/ dist start-len))
                       1.0)
        end-factor   (if (and (pos? end-len) (pos? total))
                       (let [d-from-end (- total dist)]
                         (min 1.0 (/ d-from-end end-len)))
                       1.0)]
    (min start-factor end-factor)))

;; ── 将因子应用到点参数 ────────────────────────────
(defn- apply-factors [params field-factors]
  (reduce-kv (fn [p k factor]
               (if (contains? p k)
                 (update p k * factor)
                 p))
             params
             field-factors))

;; ── 公共 API ──────────────────────────────────────

(defn taper-stroke
  "对完整笔触施加起笔与收笔锥化。
   参数：
     stroke      - {:brush brush, :params [...]}
     start-spec  - 起笔锥化规格。可以是：
                     • 数字：>1 表示像素，0~1 表示比例
                     • map：{:value v, :mode :length/:ratio}
                     • 字段 map：{:radius 10, :opacity {:value 0.1 :mode :ratio}}
     end-spec    - 收笔锥化规格，格式同 start-spec
     :fields     - 要锥化的字段向量，默认 [:radius]。
                   如果锥化规格为 map，则 fields 用于筛选（可选）。
   返回新笔触。"
  [stroke start-spec end-spec & {:keys [fields] :or {fields [:radius]}}]
  (let [{:keys [params]} stroke
        {:keys [lengths total]} (cumulative-lengths params)
        start-tapers (resolve-field-tapers fields start-spec total)
        end-tapers   (resolve-field-tapers fields end-spec total)]
    (if (and (every? zero? (vals start-tapers))
             (every? zero? (vals end-tapers)))
      stroke
      (let [new-params
            (map-indexed
              (fn [i p]
                (let [dist (nth lengths i)
                      field-factors
                      (merge-with min
                                  (into {} (map (fn [[f len]]
                                                  [f (taper-factor dist total len 0)])
                                                start-tapers))
                                  (into {} (map (fn [[f len]]
                                                  [f (taper-factor dist total 0 len)])
                                                end-tapers)))]
                  (apply-factors p field-factors)))
              params)]
        (assoc stroke :params (vec new-params))))))

(defn taper-stroke-start
  "仅应用起笔锥化（用于实时预览，笔触长度未知）。
   参数类似 taper-stroke，无需 end-spec。"
  [stroke start-spec & {:keys [fields] :or {fields [:radius]}}]
  (let [{:keys [params]} stroke
        {:keys [lengths total]} (cumulative-lengths params)
        start-tapers (resolve-field-tapers fields start-spec total)]
    (if (every? zero? (vals start-tapers))
      stroke
      (let [new-params
            (map-indexed
              (fn [i p]
                (let [dist (nth lengths i)
                      field-factors
                      (into {} (map (fn [[f len]]
                                      [f (taper-factor dist total len 0)])
                                    start-tapers))]
                  (apply-factors p field-factors)))
              params)]
        (assoc stroke :params (vec new-params))))))

(defn taper-stroke-end
  "仅应用收笔锥化（后处理）。"
  [stroke end-spec & {:keys [fields] :or {fields [:radius]}}]
  (taper-stroke stroke nil end-spec :fields fields))