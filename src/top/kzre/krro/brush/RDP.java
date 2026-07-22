package top.kzre.krro.brush;

import java.util.Arrays;

/**
 * Ramer–Douglas–Peucker 降采样算法，适用于 PointerEvent 序列。
 * <p>
 * 该实现基于坐标 (x, y) 进行简化，保留事件的其他字段不变。
 * 支持：
 * <ul>
 *   <li>最小距离预过滤：移除距离太近的点</li>
 *   <li>首尾强制保留：确保头尾指定数量的点不被简化</li>
 *   <li>递归 RDP 简化</li>
 * </ul>
 * <p>
 * 优化点：
 * <ul>
 *   <li>递归使用索引区间，避免大量子数组复制</li>
 *   <li>头部和尾部直接数组复制，避免 ArrayList 包装</li>
 *   <li>合并结果时精确分配并一次性拷贝，减少临时对象</li>
 * </ul>
 */
public final class RDP {

    private static final int DEFAULT_PRESERVE = 3; // 默认首尾保留点数

    private RDP() {} // 禁止实例化

    /**
     * 使用强制保留点进行简化。
     * <p>
     * 该方法将原始事件数组按强制保留点切分为若干子段，对每个子段独立应用 RDP 简化，
     * 确保强制保留点作为分段边界被保留。强制保留点不会被任何过滤或简化删除。
     * <p>
     * 处理流程：
     * <ol>
     *   <li>自动添加首尾索引（0 和 events.length-1）到强制点集合。</li>
     *   <li>按强制点索引排序，将数组分割为多个子段。</li>
     *   <li>对每个子段的中间部分（去除端点）应用最小距离过滤（若指定）。</li>
     *   <li>对过滤后的中间部分执行 RDP 简化。</li>
     *   <li>拼接所有端点与简化后的中间点，合并时自动去重。</li>
     * </ol>
     *
     * @param events          原始事件数组
     * @param epsilon         RDP 距离阈值（像素）
     * @param minDist         最小点间距（像素），可为 null 表示不应用
     * @param mustKeepIndices 必须保留的点的索引（不要求排序，但会内部排序去重），
     *                        如果未包含首尾，会自动添加 0 和 length-1。
     * @return 简化后的事件数组
     * @throws IllegalArgumentException 如果 events 为空或 null
     */
    public static PointerEvent[] simplifyWithMandatory(PointerEvent[] events,
                                                       float epsilon,
                                                       Float minDist,
                                                       int[] mustKeepIndices) {
        if (events == null || events.length == 0) {
            return new PointerEvent[0];
        }
        int n = events.length;

        // 构建强制点索引集合（含首尾）
        java.util.Set<Integer> idxSet = new java.util.HashSet<>();
        if (mustKeepIndices != null) {
            for (int idx : mustKeepIndices) {
                if (idx >= 0 && idx < n) idxSet.add(idx);
            }
        }
        idxSet.add(0);
        idxSet.add(n - 1);

        // 排序
        int[] indices = idxSet.stream().mapToInt(Integer::intValue).sorted().toArray();

        java.util.List<PointerEvent> result = new java.util.ArrayList<>();

        for (int i = 0; i < indices.length - 1; i++) {
            int start = indices[i];
            int end = indices[i + 1];
            if (start == end) continue;

            // 添加起点（若结果为空或与上一个不同）
            if (result.isEmpty() || result.get(result.size() - 1) != events[start]) {
                result.add(events[start]);
            }

            // 处理中间部分
            if (end - start > 1) {
                PointerEvent[] middle = new PointerEvent[end - start - 1];
                System.arraycopy(events, start + 1, middle, 0, end - start - 1);

                // 最小距离过滤（仅对中间点）
                PointerEvent[] filtered = middle;
                if (minDist != null && minDist > 0) {
                    filtered = filterClosePoints(middle, minDist);
                }

                if (filtered.length > 0) {
                    // 对中间部分执行 RDP，保留首尾（maxPoints=1）
                    PointerEvent[] simplified = rdp(filtered, 0, filtered.length, epsilon, 1);
                    // 添加除最后一个中间点外的所有点（最后一个点将在下一个循环作为端点添加）
                    result.addAll(Arrays.asList(simplified).subList(0, simplified.length - 1));
                }
            }
        }

        // 确保最后一个端点被添加（如果尚未添加）
        PointerEvent lastEvent = events[n - 1];
        if (result.isEmpty() || result.get(result.size() - 1) != lastEvent) {
            result.add(lastEvent);
        }

        return result.toArray(new PointerEvent[0]);
    }

    /**
     * 简化事件序列。
     *
     * @param events       原始事件数组
     * @param epsilon      RDP 距离阈值（像素），值越小保留的点越多
     * @param minDist      最小点间距（像素），小于此距离的点将被合并（可为 null 表示不应用）
     * @param preserveHead 头部强制保留的点数（至少为 1）
     * @param preserveTail 尾部强制保留的点数（至少为 1）
     * @return 简化后的事件数组（新对象）
     */
    public static PointerEvent[] simplify(PointerEvent[] events,
                                          float epsilon,
                                          Float minDist,
                                          int preserveHead,
                                          int preserveTail) {
        if (events == null || events.length == 0) {
            return new PointerEvent[0];
        }

        // 1. 最小距离预过滤
        PointerEvent[] filtered = events;
        if (minDist != null && minDist > 0) {
            filtered = filterClosePoints(events, minDist);
        }

        if (filtered.length == 0 || epsilon <= 0) {
            return filtered;
        }

        // 2. 执行分段 RDP
        return rdpSegment(filtered, epsilon, preserveHead, preserveTail);
    }

    /**
     * 简化版本，使用默认首尾保留点数（各 3 个）。
     */
    public static PointerEvent[] simplify(PointerEvent[] events, float epsilon, Float minDist) {
        return simplify(events, epsilon, minDist, DEFAULT_PRESERVE, DEFAULT_PRESERVE);
    }

    /**
     * 不使用最小距离过滤的版本。
     */
    public static PointerEvent[] simplify(PointerEvent[] events, float epsilon) {
        return simplify(events, epsilon, null, DEFAULT_PRESERVE, DEFAULT_PRESERVE);
    }

    // ======================== 私有辅助方法 ========================

    /**
     * 过滤距离太近的点，保留第一个，跳过后续太近的点。
     * 优化：使用数组+计数器，避免 ArrayList 开销。
     */
    private static PointerEvent[] filterClosePoints(PointerEvent[] events, float minDist) {
        if (events.length == 0) return events;
        // 预先分配和 events 一样大的数组，用 count 跟踪实际数量
        PointerEvent[] result = new PointerEvent[events.length];
        result[0] = events[0];
        int count = 1;
        for (int i = 1; i < events.length; i++) {
            PointerEvent last = result[count - 1];
            PointerEvent cur = events[i];
            float dx = cur.getX() - last.getX();
            float dy = cur.getY() - last.getY();
            // 用平方距离比较，避免 sqrt
            if (dx * dx + dy * dy > minDist * minDist) {
                result[count++] = cur;
            }
        }
        // 截断多余部分
        if (count == events.length) {
            return events; // 没有过滤掉任何点，直接返回原数组（保持不可变，但调用者不会修改）
        }
        PointerEvent[] trimmed = new PointerEvent[count];
        System.arraycopy(result, 0, trimmed, 0, count);
        return trimmed;
    }

    /**
     * 计算点 (x, y) 到线段 (x1,y1)-(x2,y2) 的垂直距离。
     * 使用叉积 / 模长，并避免除零。
     */
    private static float perpendicularDistance(PointerEvent p,
                                               PointerEvent a,
                                               PointerEvent b) {
        float x = p.getX(), y = p.getY();
        float x1 = a.getX(), y1 = a.getY();
        float x2 = b.getX(), y2 = b.getY();
        float dx = x2 - x1;
        float dy = y2 - y1;
        float mag2 = dx * dx + dy * dy;
        if (mag2 == 0) {
            // 线段退化为点，返回点到第一个点的欧氏距离
            return (float) Math.sqrt((x - x1) * (x - x1) + (y - y1) * (y - y1));
        }
        // 叉积绝对值 / 模长
        float cross = dy * x - dx * y - dy * x1 + dx * y1;
        return Math.abs(cross) / (float) Math.sqrt(mag2);
    }

    /**
     * 递归 RDP 简化（使用索引区间，避免子数组复制）。
     * @param src    原始数组
     * @param from   起始索引（包含）
     * @param to     结束索引（不包含）
     * @param epsilon 距离阈值
     * @param maxPoints 最小保留点数（防止过度简化）
     * @return 简化后的点数组（新对象）
     */
    private static PointerEvent[] rdp(PointerEvent[] src, int from, int to,
                                      float epsilon, int maxPoints) {
        int len = to - from;
        if (len <= maxPoints) {
            // 区间长度 ≤ maxPoints，直接复制返回
            PointerEvent[] res = new PointerEvent[len];
            System.arraycopy(src, from, res, 0, len);
            return res;
        }
        PointerEvent first = src[from];
        PointerEvent last = src[to - 1];
        int index = -1;
        float dmax = 0;
        for (int i = from + 1; i < to - 1; i++) {
            float d = perpendicularDistance(src[i], first, last);
            if (d > dmax) {
                dmax = d;
                index = i;
            }
        }
        if (dmax > epsilon) {
            // 递归处理左右两部分
            PointerEvent[] left = rdp(src, from, index + 1, epsilon, maxPoints);
            PointerEvent[] right = rdp(src, index, to, epsilon, maxPoints);
            // 合并：left + right[1..] （避免重复中间点）
            PointerEvent[] result = new PointerEvent[left.length + right.length - 1];
            System.arraycopy(left, 0, result, 0, left.length);
            // 跳过 right 的第一个元素（即中间点，已在 left 的末尾）
            System.arraycopy(right, 1, result, left.length, right.length - 1);
            return result;
        } else {
            // 保留首尾
            return new PointerEvent[]{first, last};
        }
    }

    /**
     * 对中间部分应用 RDP，同时确保首尾保留指定数量的点。
     * 优化：直接数组复制头部和尾部，避免 ArrayList。
     */
    private static PointerEvent[] rdpSegment(PointerEvent[] events,
                                             float epsilon,
                                             int preserveHead,
                                             int preserveTail) {
        int total = events.length;
        if (total == 0) return events;

        // 修正 preserveHead/preserveTail 范围
        int head = Math.min(preserveHead, total);
        int tail = Math.min(preserveTail, total - head);
        if (head < 0) head = 0;
        if (tail < 0) tail = 0;

        // 如果首尾保留覆盖整个序列，直接复制返回
        if (head + tail >= total) {
            return events.clone();
        }

        // 复制头部
        PointerEvent[] headArr = new PointerEvent[head];
        System.arraycopy(events, 0, headArr, 0, head);

        // 复制尾部
        PointerEvent[] tailArr = new PointerEvent[tail];
        System.arraycopy(events, total - tail, tailArr, 0, tail);

        // 中间部分直接使用原始数组的引用（不复制），由 rdp 内部按需复制
        int midStart = head;
        int midEnd = total - tail;
        // 对中间部分进行 RDP，maxPoints 至少为 1，防止完全删空
        PointerEvent[] simplifiedMiddle = rdp(events, midStart, midEnd, epsilon, Math.max(1, head + tail));

        // 合并：headArr + simplifiedMiddle + tailArr
        PointerEvent[] result = new PointerEvent[head + simplifiedMiddle.length + tail];
        System.arraycopy(headArr, 0, result, 0, head);
        System.arraycopy(simplifiedMiddle, 0, result, head, simplifiedMiddle.length);
        System.arraycopy(tailArr, 0, result, head + simplifiedMiddle.length, tail);
        return result;
    }
}