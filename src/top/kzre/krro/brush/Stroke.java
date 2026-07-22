package top.kzre.krro.brush;

import top.kzre.colorutils.color.RGB;
import top.kzre.colorutils.blend.Blends;
import top.kzre.krro.util.tile.Canvas;
import top.kzre.krro.util.tile.RandomAccessIterator;
import top.kzre.krro.util.tile.TiledCanvas;

import java.util.HashSet;
import java.util.Set;

public final class Stroke {

    private Stroke() {}

    /**
     * 将 dab 遮罩通过 {@link Canvas} 的随机访问迭代器直接混合到画布上。
     * 此方法无需快照或拷贝瓦片，性能极高，且与视图（CanvasView）无缝协作。
     *
     * @param canvas        目标画布（或其视图）
     * @param maskData      dab 遮罩数据
     * @param maskW         dab 宽度
     * @param maskH         dab 高度
     * @param cx            dab 中心世界 X 坐标
     * @param cy            dab 中心世界 Y 坐标
     * @param fgColor       前景色 RGBA
     * @param blendMode     混合模式
     * @param extraOpacity  额外不透明度
     * @param postProcessor 像素后处理器（可为 null）
     * @return 受影响的瓦片键集合
     */
    public static Set<Long> stampDab(Canvas canvas,
                                     float[] maskData, int maskW, int maskH,
                                     int cx, int cy,
                                     float[] fgColor, String blendMode, float extraOpacity,
                                     PixelPostprocessor postProcessor) {
        int tileSize = canvas.getTileSize();
        RandomAccessIterator it = canvas.createRandomAccessIterator(true); // 可写
        Set<Long> dirtySet = new HashSet<>();
        int halfW = maskW / 2;
        int halfH = maskH / 2;
        float baseAlpha = RGB.alpha(fgColor);

        for (int py = 0; py < maskH; py++) {
            int canvasY = cy - halfH + py;
            int ty = Math.floorDiv(canvasY, tileSize);

            for (int px = 0; px < maskW; px++) {
                float maskAlpha = maskData[py * maskW + px];
                if (maskAlpha <= 0.0) continue;
                int canvasX = cx - halfW + px;
                int tx = Math.floorDiv(canvasX, tileSize);

                it.moveTo(canvasX, canvasY);
                float[] bg = new float[4];
                it.getPixel(bg);

                float pixelAlpha = maskAlpha * extraOpacity;
                float[] src = RGB.withAlpha(fgColor, baseAlpha * pixelAlpha);
                float[] blended = Blends.blendWithAlpha(blendMode, bg, src);

                if (postProcessor != null) {
                    blended = postProcessor.process(blended);
                }

                it.setPixel(blended[0], blended[1], blended[2], blended[3]);

                long key = TiledCanvas.pack(tx, ty);
                dirtySet.add(key);
            }
        }
        return dirtySet;
    }
}