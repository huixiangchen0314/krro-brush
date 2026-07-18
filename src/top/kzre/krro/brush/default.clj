(ns top.kzre.krro.brush.default
  "笔刷默认参数补全。")

(defn apply-defaults
  "为事件序列中的每个事件补充笔刷默认参数，确保 :radius, :opacity 等字段存在。
   brush-spec 中应包含这些默认值。"
  [events brush-spec]
  (let [defaults {:radius  (double (or (:radius brush-spec) 5))
                  :opacity 1.0
                  :flow    1.0}]
    (mapv #(merge defaults %) events)))