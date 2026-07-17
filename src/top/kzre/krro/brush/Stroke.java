package top.kzre.krro.brush;

import top.kzre.colorutils.color.RGB;
import top.kzre.colorutils.blend.Blends;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public final class Stroke {

    private Stroke() {}

    /**
     * 将 dab 遮罩混合到瓦片画布上，无边界裁剪，允许任意世界坐标。
     *
     * @param tiles        瓦片映射，键为位编码 (tx, ty)，值为 float[RGBA * tileSize * tileSize]
     * @param tileSize     瓦片边长（像素）
     * @param maskData     dab 遮罩数据（float[]，长度 = maskW * maskH）
     * @param maskW        dab 宽度
     * @param maskH        dab 高度
     * @param cx           dab 中心在世界坐标中的 x（像素）
     * @param cy           dab 中心在世界坐标中的 y（像素）
     * @param fgColor      前景色 RGBA float[4]
     * @param blendMode    混合模式名称
     * @param extraOpacity 额外不透明度 (0~1)
     * @param postProcessor 像素后处理器
     * @return 受影响的瓦片键集合（不会为 null）
     */
    public static Set<Long> stampDabTiled(Map<Long, float[]> tiles, int tileSize,
                                          float[] maskData, int maskW, int maskH,
                                          int cx, int cy,
                                          float[] fgColor, String blendMode, float extraOpacity,
                                          PixelPostprocessor postProcessor) {
        Set<Long> dirtySet = new HashSet<>();
        int halfW = maskW / 2;
        int halfH = maskH / 2;
        float baseAlpha = RGB.alpha(fgColor);

        for (int py = 0; py < maskH; py++) {
            int canvasY = cy - halfH + py;
            // 使用 floorDiv 处理负坐标
            int ty = Math.floorDiv(canvasY, tileSize);
            int localY = Math.floorMod(canvasY, tileSize);

            for (int px = 0; px < maskW; px++) {
                float maskAlpha = maskData[py * maskW + px];
                if (maskAlpha <= 0.0) continue;
                int canvasX = cx - halfW + px;
                int tx = Math.floorDiv(canvasX, tileSize);
                int localX = Math.floorMod(canvasX, tileSize);

                // 位编码：高32位 tx，低32位 ty（无符号）
                long key = ((long) tx << 32) | (ty & 0xFFFFFFFFL);
                float[] tile = tiles.computeIfAbsent(key, k -> new float[tileSize * tileSize * 4]);

                int idx = 4 * (localY * tileSize + localX);
                float[] bg = {tile[idx], tile[idx+1], tile[idx+2], tile[idx+3]};
                float pixelAlpha = maskAlpha * extraOpacity;
                float[] src = RGB.withAlpha(fgColor, baseAlpha * pixelAlpha);
                float[] blended = Blends.blendWithAlpha(blendMode, bg, src);

                if (postProcessor != null) {
                    blended = postProcessor.process(blended);
                }

                tile[idx]   = blended[0];
                tile[idx+1] = blended[1];
                tile[idx+2] = blended[2];
                tile[idx+3] = blended[3];

                dirtySet.add(key);
            }
        }

        return dirtySet;
    }

    /**
     * 将 dab 遮罩与画布混合（原地修改），返回脏矩形 [x, y, width, height]。
     *
     * @param canvas      画布 RGBA 数组 (w * h * 4)
     * @param w           画布宽度
     * @param h           画布高度
     * @param maskData    dab 遮罩数据（float[], 长度 = maskW * maskH）
     * @param maskW       dab 宽度
     * @param maskH       dab 高度
     * @param cx          dab 中心在画布上的 x 坐标
     * @param cy          dab 中心在画布上的 y 坐标
     * @param fgColor     前景色 RGBA float[4]
     * @param blendMode   混合模式名称（如 "normal", "multiply"）
     * @param extraOpacity 额外不透明度（0..1）
     * @return 脏矩形 [x, y, width, height]，若无有效绘制返回 null
     */
    public static int[] stampDab(float[] canvas, int w, int h,
                                 float[] maskData, int maskW, int maskH,
                                 int cx, int cy,
                                 float[] fgColor, String blendMode, float extraOpacity,
                                 PixelPostprocessor postProcessor) {
        int halfW = maskW / 2;
        int halfH = maskH / 2;
        int dirtyMinX = Integer.MAX_VALUE, dirtyMinY = Integer.MAX_VALUE;
        int dirtyMaxX = Integer.MIN_VALUE, dirtyMaxY = Integer.MIN_VALUE;
        float baseAlpha = RGB.alpha(fgColor);

        for (int py = 0; py < maskH; py++) {
            int rowStart = py * maskW;
            int canvasY = cy - halfH + py;
            if (canvasY < 0 || canvasY >= h) continue;

            for (int px = 0; px < maskW; px++) {
                float maskAlpha = maskData[rowStart + px];
                if (maskAlpha <= 0.0) continue;

                int canvasX = cx - halfW + px;
                if (canvasX < 0 || canvasX >= w) continue;

                int idx = 4 * (canvasX + canvasY * w);
                float[] bg = {canvas[idx], canvas[idx+1], canvas[idx+2], canvas[idx+3]};
                float pixelAlpha = maskAlpha * extraOpacity;
                float[] src = RGB.withAlpha(fgColor, baseAlpha * pixelAlpha);
                float[] blended = Blends.blendWithAlpha(blendMode, bg, src);

                // 后处理（可原地修改 blended）
                if (postProcessor != null) {
                    blended = postProcessor.process(blended);
                }

                canvas[idx]   = blended[0];
                canvas[idx+1] = blended[1];
                canvas[idx+2] = blended[2];
                canvas[idx+3] = blended[3];

                if (canvasX < dirtyMinX) dirtyMinX = canvasX;
                if (canvasX > dirtyMaxX) dirtyMaxX = canvasX;
                if (canvasY < dirtyMinY) dirtyMinY = canvasY;
                if (canvasY > dirtyMaxY) dirtyMaxY = canvasY;
            }
        }
        if (dirtyMinX == Integer.MAX_VALUE) return null;
        return new int[]{dirtyMinX, dirtyMinY,
                dirtyMaxX - dirtyMinX + 1,
                dirtyMaxY - dirtyMinY + 1};
    }
}