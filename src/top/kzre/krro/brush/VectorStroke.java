package top.kzre.krro.brush;

import top.kzre.curve.bezier2d.Bezier2D;
import top.kzre.curve.bezier2d.ChordLengthTable;
import top.kzre.curve.bezier2d.Curve;

/**
 * 矢量笔触生成器：拟合贝塞尔曲线并沿弦长等距采样宽度。
 * 完全基于已有的 bezier2d 库，代码极简。
 */
public final class VectorStroke {

    private VectorStroke() {}

    /**
     * @param xs       输入点 x 坐标
     * @param ys       输入点 y 坐标
     * @param widths   每个输入点对应的宽度（由动力学提前计算好）
     * @param maxError 贝塞尔拟合最大误差
     * @param samples  宽度采样数量（返回 samples+1 个均匀采样点）
     */
    public static VectorStrokeResult generate(
            double[] xs, double[] ys, double[] widths,
            double maxError, int samples) {

        if (xs.length != ys.length || xs.length != widths.length)
            throw new IllegalArgumentException("xs, ys, widths must have same length");

        if (xs.length < 2) {
            // 退化为单点，返回零长度曲线和常值宽度
            double[] w = new double[samples + 1];
            double[] a = new double[samples + 1];
            for (int i = 0; i <= samples; i++) {
                a[i] = (double) i / samples;
                w[i] = widths.length > 0 ? widths[0] : 1.0;
            }
            Curve degenerate = Bezier2D.fit(new double[]{xs[0], xs[0]},
                    new double[]{ys[0], ys[0]}, maxError, 1);
            return new VectorStrokeResult(degenerate, w, a);
        }

        // 1. 拟合曲线
        Curve curve = Bezier2D.fit(xs, ys, maxError, 100);

        // 2. 原始点弦长参数化
        ChordLengthTable chordTable = new ChordLengthTable(xs, ys);
        double[] tParams = chordTable.getParameters();  // 每个原始点的归一化弦长参数

        // 3. 均匀采样参数，并对宽度线性插值
        double[] sampledWidths = new double[samples + 1];
        double[] sampledArcParams = new double[samples + 1];
        for (int i = 0; i <= samples; i++) {
            double t = (double) i / samples;
            sampledArcParams[i] = t;
            sampledWidths[i] = interpolate(tParams, widths, t);
        }

        return new VectorStrokeResult(curve, sampledWidths, sampledArcParams);
    }

    /** 在单调递增的 xs 和对应 ys 之间线性插值 */
    private static double interpolate(double[] xs, double[] ys, double x) {
        int n = xs.length;
        if (x <= xs[0]) return ys[0];
        if (x >= xs[n - 1]) return ys[n - 1];
        int idx = 0;
        while (idx < n - 1 && xs[idx + 1] < x) idx++;
        double t = (x - xs[idx]) / (xs[idx + 1] - xs[idx]);
        return ys[idx] + t * (ys[idx + 1] - ys[idx]);
    }

    // ── 结果对象 ─────────────────────────────────
    public static class VectorStrokeResult {
        private final Curve curve;
        private final double[] widthSamples;
        private final double[] arcParams;

        public VectorStrokeResult(Curve curve, double[] widthSamples, double[] arcParams) {
            this.curve = curve;
            this.widthSamples = widthSamples;
            this.arcParams = arcParams;
        }

        public Curve getCurve()           { return curve; }
        public double[] getWidthSamples() { return widthSamples; }
        public double[] getArcParams()    { return arcParams; }
    }
}