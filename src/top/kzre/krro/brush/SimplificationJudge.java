package top.kzre.krro.brush;

/**
 * 事件简化评估工具类，用于量化降采样算法（如 RDP、CBS 等）的效果。
 * 提供偏差统计、压缩率等指标，便于调参与算法对比。
 */
public final class SimplificationJudge {

    private SimplificationJudge() {} // 禁止实例化

    /**
     * 偏差统计结果。
     */
    public static class DeviationStats {
        public final float maxDeviation;   // 最大偏差（像素）
        public final float avgDeviation;   // 平均偏差（像素）
        public final float rmseDeviation;  // 均方根偏差（像素）
        public final float compressionRate; // 压缩率 (1 - simplified/original)

        private DeviationStats(float maxDev, float avgDev, float rmseDev,
                               float compRate) {
            this.maxDeviation = maxDev;
            this.avgDeviation = avgDev;
            this.rmseDeviation = rmseDev;
            this.compressionRate = compRate;
        }

        @Override
        public String toString() {
            return String.format(
                    "DeviationStats{max=%.3f, avg=%.3f, rmse=%.3f, compression=%.1f%%}",
                    maxDeviation, avgDeviation, rmseDeviation, compressionRate * 100
            );
        }
    }

    /**
     * 计算简化曲线的偏差统计及压缩率。
     *
     * @param original   原始点序列（不可为 null 或空）
     * @param simplified 简化后点序列（不可为 null 或空）
     * @return 包含最大偏差、平均偏差、RMSE 和压缩率的 DeviationStats 对象
     * @throws IllegalArgumentException 如果任一数组为 null 或空
     */
    public static DeviationStats computeDeviationStats(PointerEvent[] original,
                                                       PointerEvent[] simplified) {
        if (original == null || original.length == 0) {
            throw new IllegalArgumentException("原始点序列不能为空");
        }
        if (simplified == null || simplified.length == 0) {
            throw new IllegalArgumentException("简化点序列不能为空");
        }

        // 计算每个原始点到简化曲线的最近距离
        float maxDev = 0f;
        float sumDev = 0f;
        float sumSq = 0f;
        int n = original.length;

        for (PointerEvent p : original) {
            float dist = distanceToPolyline(p, simplified);
            maxDev = Math.max(maxDev, dist);
            sumDev += dist;
            sumSq += dist * dist;
        }

        float avgDev = sumDev / n;
        float rmse = (float) Math.sqrt(sumSq / n);
        float compRate = 1f - ((float) simplified.length / original.length);

        return new DeviationStats(maxDev, avgDev, rmse, compRate);
    }

    /**
     * 计算点 p 到折线（由 points 数组定义）的最短距离。
     * 若折线仅有一个点，则返回点到该点的距离。
     *
     * @param p      待测点
     * @param points 折线顶点（至少包含一个点）
     * @return 点到折线的最短距离
     */
    private static float distanceToPolyline(PointerEvent p, PointerEvent[] points) {
        int m = points.length;
        if (m == 1) {
            return distance(p, points[0]);
        }

        float minDist = Float.MAX_VALUE;
        for (int i = 0; i < m - 1; i++) {
            float d = distanceToSegment(p, points[i], points[i + 1]);
            if (d < minDist) minDist = d;
        }
        return minDist;
    }

    /**
     * 计算点 p 到线段 (a, b) 的垂直距离。
     * 若线段退化为点，则返回点到该点的距离。
     */
    private static float distanceToSegment(PointerEvent p, PointerEvent a, PointerEvent b) {
        float x = p.getX(), y = p.getY();
        float x1 = a.getX(), y1 = a.getY();
        float x2 = b.getX(), y2 = b.getY();
        float dx = x2 - x1;
        float dy = y2 - y1;
        float mag2 = dx * dx + dy * dy;
        if (mag2 == 0) {
            // 线段退化为点
            return (float) Math.sqrt((x - x1) * (x - x1) + (y - y1) * (y - y1));
        }
        // 投影参数 t = dot(p-a, b-a) / |b-a|^2，限制在 [0,1] 内
        float t = ((x - x1) * dx + (y - y1) * dy) / mag2;
        t = Math.max(0f, Math.min(1f, t));
        // 投影点坐标
        float projX = x1 + t * dx;
        float projY = y1 + t * dy;
        return (float) Math.sqrt((x - projX) * (x - projX) + (y - projY) * (y - projY));
    }

    /**
     * 计算两点间欧氏距离。
     */
    private static float distance(PointerEvent a, PointerEvent b) {
        float dx = a.getX() - b.getX();
        float dy = a.getY() - b.getY();
        return (float) Math.sqrt(dx * dx + dy * dy);
    }

    // ==================== 快捷打印方法 ====================

    /**
     * 打印偏差统计信息到标准输出。
     */
    public static void printStats(PointerEvent[] original, PointerEvent[] simplified) {
        DeviationStats stats = computeDeviationStats(original, simplified);
        System.out.println("=== Simplification Stats ===");
        System.out.println("Original points: " + original.length);
        System.out.println("Simplified points: " + simplified.length);
        System.out.println(stats);
    }
}