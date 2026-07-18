(ns top.kzre.krro.brush.taper
  "基于 :length 的锥化模块。
   必须配合 measure 模块使用，事件必须包含 :length 字段。
   运行时不做校验，由调用方保证数据完整性。")

(defn taper-events
  "对事件向量应用锥化，返回新的事件向量。
   参数：
     events              - 事件向量，每个事件必须包含 :length (由 measure 提供)
     :taper-start-px     - 起笔锥化像素长度
     :taper-end-px       - 收笔锥化像素长度
     :taper-start-ratio  - 起笔锥化比例（相对于总长，与像素二选一）
     :taper-end-ratio    - 收笔锥化比例
     :taper-fields       - 锥化影响的字段，默认 [:radius :opacity]
   若未提供任何锥化参数，返回原序列。"
  [events & {:keys [taper-start-px taper-end-px
                    taper-start-ratio taper-end-ratio
                    taper-fields]
             :or   {taper-start-ratio 0.0, taper-end-ratio 0.0
                    taper-fields [:radius :opacity]}}]
  (let [total  (double (:length (last events)))
        start  (double (or taper-start-px (* total taper-start-ratio)))
        end    (double (or taper-end-px   (* total taper-end-ratio)))
        fields (vec taper-fields)]
    (if (and (zero? start) (zero? end))
      events
      (mapv (fn [ev]
              (let [dist   (:length ev)
                    from-start dist
                    from-end   (- total dist)
                    start-factor (if (pos? start) (min 1.0 (/ from-start start)) 1.0)
                    end-factor   (if (pos? end)   (min 1.0 (/ from-end end))   1.0)
                    factor       (min start-factor end-factor)]
                (reduce (fn [m f] (if (contains? m f)
                                    (update m f #(* (double %) factor))
                                    m))
                        ev
                        fields)))
            events))))

(defn taper-stroke
  "对 stroke map 的 :params 向量进行锥化，返回新的 stroke map。
   stroke 结构: {:brush ... :params [events]}
   可选参数完全透传给 taper-events，即：
     :taper-start-px, :taper-end-px, :taper-start-ratio, :taper-end-ratio, :taper-fields"
  [stroke & opts]
  (update stroke :params #(apply taper-events % opts)))

(defn taper-stroke-start
  "仅起笔锥化，收笔锥化长度为 0。"
  [stroke & {:keys [taper-start-px taper-start-ratio taper-fields]
             :or {taper-fields [:radius :opacity]}}]
  (taper-stroke stroke
                :taper-start-px taper-start-px
                :taper-start-ratio (or taper-start-ratio 0.0)
                :taper-end-px 0
                :taper-end-ratio 0.0
                :taper-fields taper-fields))

(defn taper-stroke-end
  "仅收笔锥化，起笔锥化长度为 0。"
  [stroke & {:keys [taper-end-px taper-end-ratio taper-fields]
             :or {taper-fields [:radius :opacity]}}]
  (taper-stroke stroke
                :taper-start-px 0
                :taper-start-ratio 0.0
                :taper-end-px taper-end-px
                :taper-end-ratio (or taper-end-ratio 0.0)
                :taper-fields taper-fields))