(ns top.kzre.krro.brush.stroke
  "笔触状态管理：维护在一次笔触内跨 dab 传递的可变状态，
   如 SAI 混色中的 carry-color。状态本身是普通 Clojure map，
   所有更新均为纯函数，返回新状态。")

(defn init-state
  "返回一个初始的笔触状态 map。
   当前支持的键：
     :carry-color - 当前携带的颜色 [r g b] 或 nil
     :last-pos    - 上一个 dab 的中心位置 [x y] 或 nil"
  []
  {:carry-color nil
   :last-pos    nil})

(defn update-state
  "根据传入的键值对更新笔触状态，返回新状态。
   attrs 为 map，可包含 :carry-color, :last-pos。
   不修改传入的 state。"
  [state attrs]
  (merge state attrs))