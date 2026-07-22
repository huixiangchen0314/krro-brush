package top.kzre.krro.brush;

import java.util.ArrayList;
import java.util.List;

/**
 * CBS (Corner-Based Simplifier) —— 基于局部弯曲程度的角点简化器。
 * <p>
 * 该算法通过计算点序列中每个点前后线段的夹角，识别并保留转折尖锐的角点，
 * 同时剔除处于平直或平滑弧段上的冗余点，适用于笔迹、折线等数据的降采样与关键特征提取。
 * <p>
 * 支持两种工作模式：
 * <ul>
 *   <li><b>批量处理</b>：一次性对完整点数组进行全局简化。</li>
 *   <li><b>流式处理</b>：增量接收输入点，实时维护内部状态，可在任意时刻输出当前检测到的角点。</li>
 * </ul>
 * <p>
 * 算法核心步骤：
 * <ol>
 *   <li>对于每个中间点 P<sub>i</sub>，取前一点 P<sub>i-1</sub> 和后一点 P<sub>i+1</sub>，</li>
 *   <li>计算向量 P<sub>i-1</sub>→P<sub>i</sub> 与 P<sub>i+1</sub>→P<sub>i</sub> 的夹角 θ（弧度），</li>
 *   <li>若 θ < 设定的角度阈值（弧度），则将 P<sub>i</sub> 标记为角点（重要特征点），</li>
 *   <li>首尾点始终强制保留，确保笔画完整性。</li>
 * </ol>
 * <p>
 * 阈值选择建议：
 * <ul>
 *   <li>角度阈值 <b>越小</b>，保留的角点越少，简化越激进（仅保留最尖锐的拐点）。</li>
 *   <li>角度阈值 <b>越大</b>，保留的角点越多，细节保留更完整。</li>
 *   <li>推荐范围：<b>120° ~ 150°</b>（经验值），可根据实际应用调整。</li>
 * </ul>
 * <p>
 * 该类设计为线程安全（无共享可变状态），每个实例独立维护自己的点队列和角点标记。
 *
 * @see RDP 另一种基于全局形状逼近的降采样算法
 */
public final class CBS {

    private final float angleThresholdRad; // 角度阈值（弧度）
    private final List<PointerEvent> points = new ArrayList<>();
    private boolean[] isCorner; // 标记角点，动态扩展

    // ==================== 构造与工厂方法 ====================

    private CBS(float angleThresholdDegrees) {
        this.angleThresholdRad = (float) Math.toRadians(angleThresholdDegrees);
    }

    /**
     * 创建流式检测器实例。
     *
     * @param angleThresholdDegrees 角度阈值（度），小于该值视为拐点，推荐 120~150°
     * @return CBS 实例
     */
    public static CBS create(float angleThresholdDegrees) {
        return new CBS(angleThresholdDegrees);
    }

    // ==================== 流式接口 ====================

    /**
     * 添加一个事件点，内部自动判断是否形成角点。
     * <p>
     * 每加入一个新点，会立即检查前一个点（即新点加入前的最后一个点），
     * 因为此时该点已有完整的前后上下文。
     *
     * @param e 新事件（不可为 null）
     */
    public void addEvent(PointerEvent e) {
        points.add(e);
        int size = points.size();
        if (size >= 3) {
            int idx = size - 2; // 判断点索引（倒数第二个点）
            if (idx <= size - 2) {
                float angle = computeAngle(points.get(idx - 1), points.get(idx), points.get(idx + 1));
                if (angle < angleThresholdRad) {
                    ensureCornerArray(idx);
                    isCorner[idx] = true;
                }
            }
        }
    }

    /**
     * 结束流式输入，返回所有检测到的角点（强制包含首尾点）。
     *
     * @return 角点数组（按原始顺序），若无任何点则返回空数组
     */
    public PointerEvent[] finish() {
        if (points.isEmpty()) return new PointerEvent[0];
        // 强制首尾为角点
        ensureCornerArray(0);
        isCorner[0] = true;
        int last = points.size() - 1;
        ensureCornerArray(last);
        isCorner[last] = true;

        List<PointerEvent> result = new ArrayList<>();
        for (int i = 0; i < points.size(); i++) {
            if (isCorner[i]) result.add(points.get(i));
        }
        return result.toArray(new PointerEvent[0]);
    }

    /**
     * 重置内部状态，清空所有缓存，以便重新开始新的流式处理。
     */
    public void reset() {
        points.clear();
        isCorner = null;
    }

    // ==================== 批量处理（静态方法） ====================

    /**
     * 批量角点检测（非流式），用于一次性简化完整的事件数组。
     *
     * @param events                  原始事件数组（可为 null 或空）
     * @param angleThresholdDegrees   角度阈值（度）
     * @return 简化后的事件数组（角点），若输入无效则返回空数组
     */
    public static PointerEvent[] simplify(PointerEvent[] events, float angleThresholdDegrees) {
        if (events == null || events.length < 3) {
            return events != null ? events.clone() : new PointerEvent[0];
        }

        float thresholdRad = (float) Math.toRadians(angleThresholdDegrees);
        int n = events.length;
        boolean[] corner = new boolean[n];
        corner[0] = true;
        corner[n - 1] = true;

        for (int i = 1; i < n - 1; i++) {
            float angle = computeAngle(events[i - 1], events[i], events[i + 1]);
            if (angle < thresholdRad) {
                corner[i] = true;
            }
        }

        List<PointerEvent> result = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            if (corner[i]) result.add(events[i]);
        }
        return result.toArray(new PointerEvent[0]);
    }

    // ==================== 私有辅助方法 ====================

    /**
     * 计算三点构成的夹角（弧度），以中间点 b 为顶点。
     * 返回值为 0～π 之间的弧度。
     */
    private static float computeAngle(PointerEvent a, PointerEvent b, PointerEvent c) {
        float ax = a.getX() - b.getX(), ay = a.getY() - b.getY();
        float cx = c.getX() - b.getX(), cy = c.getY() - b.getY();
        float dot = ax * cx + ay * cy;
        float cross = ax * cy - ay * cx;
        return (float) Math.atan2(Math.abs(cross), dot);
    }

    /**
     * 确保 isCorner 数组容量至少为 points.size()。
     */
    private void ensureCornerArray(int idx) {
        int size = points.size();
        if (isCorner == null) {
            isCorner = new boolean[size];
        } else if (idx >= isCorner.length) {
            boolean[] newArr = new boolean[size];
            System.arraycopy(isCorner, 0, newArr, 0, isCorner.length);
            isCorner = newArr;
        }
    }
}