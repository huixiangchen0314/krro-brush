package top.kzre.krro.brush;

import top.kzre.colorutils.color.RGB;

public final class Mix {

    private Mix() {}

    /**
     * 在 RGBA 浮点数组 data 上对以 (cx,cy) 为中心、半径 radius 的区域进行高斯加权颜色采样。
     *
     * @param data   画布数组，RGBA 交错存储，长度为 w * h * 4
     * @param w      画布宽度
     * @param h      画布高度
     * @param cx     采样中心 x
     * @param cy     采样中心 y
     * @param radius 采样半径
     * @return RGBA float[4]，各分量 0~1
     */
    public static float[] sampleGaussian(float[] data, int w, int h,
                                         float cx, float cy, float radius) {
        if (radius <= 0.0f) {
            int x = (int) cx;
            int y = (int) cy;
            int idx = 4 * (x + y * w);
            return RGB.rgba(data[idx], data[idx + 1], data[idx + 2], data[idx + 3]);
        }

        int r = (int) Math.ceil(radius);
        float sigma = radius / 2.0f;
        float sumR = 0.0f, sumG = 0.0f, sumB = 0.0f, sumA = 0.0f, sumW = 0.0f;

        int minY = Math.max(0, (int) cy - r);
        int maxY = Math.min(h, (int) cy + r + 1);
        int minX = Math.max(0, (int) cx - r);
        int maxX = Math.min(w, (int) cx + r + 1);

        for (int y = minY; y < maxY; y++) {
            for (int x = minX; x < maxX; x++) {
                float dx = x - cx;
                float dy = y - cy;
                float dist2 = dx * dx + dy * dy;
                float dist = (float) Math.sqrt(dist2);
                if (dist <= radius) {
                    float weight = (float) Math.exp(-0.5f * dist2 / (sigma * sigma));
                    int idx = 4 * (x + y * w);
                    sumR += weight * data[idx];
                    sumG += weight * data[idx + 1];
                    sumB += weight * data[idx + 2];
                    sumA += weight * data[idx + 3];
                    sumW += weight;
                }
            }
        }

        float wgt = Math.max(sumW, 1e-12f);
        return RGB.rgba(sumR / wgt, sumG / wgt, sumB / wgt, sumA / wgt);
    }
}