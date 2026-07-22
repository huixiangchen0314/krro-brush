package top.kzre.krro.brush;

import top.kzre.colorutils.color.RGB;
import top.kzre.krro.util.tile.TiledCanvas;

import java.util.Map;

public final class Mix {

    private Mix() {}

    /**
     * 在瓦片画布上对以 (cx,cy) 为中心、半径 radius 的区域进行高斯加权颜色采样。
     * 超出已分配瓦片范围的像素视为透明 (0,0,0,0)。
     */
    public static float[] sampleGaussianTiled(Map<Long, float[]> tiles, int tileSize,
                                              float cx, float cy, float radius) {
        if (radius <= 0.0f) {
            int ix = Math.round(cx);
            int iy = Math.round(cy);
            return samplePixelTiled(tiles, tileSize, ix, iy);
        }

        int r = (int) Math.ceil(radius);
        float sigma = radius / 2.0f;
        float sumR = 0.0f, sumG = 0.0f, sumB = 0.0f, sumA = 0.0f, sumW = 0.0f;

        int minY = (int) (cy - r);
        int maxY = (int) (cy + r);
        int minX = (int) (cx - r);
        int maxX = (int) (cx + r);

        for (int y = minY; y <= maxY; y++) {
            int ty = TiledCanvas.tileY(y, tileSize);
            int localY = TiledCanvas.localY(y, tileSize);

            for (int x = minX; x <= maxX; x++) {
                float dx = x - cx;
                float dy = y - cy;
                float dist2 = dx * dx + dy * dy;
                if (dist2 > radius * radius) continue;

                int tx = TiledCanvas.tileX(x, tileSize);
                int localX = TiledCanvas.localX(x, tileSize);
                long key = TiledCanvas.pack(tx, ty);

                float[] tile = tiles.get(key);
                if (tile == null) continue;

                int idx = 4 * (localY * tileSize + localX);
                float rPix = tile[idx];
                float gPix = tile[idx + 1];
                float bPix = tile[idx + 2];
                float aPix = tile[idx + 3];

                float weight = (float) Math.exp(-0.5f * dist2 / (sigma * sigma));
                sumR += weight * rPix;
                sumG += weight * gPix;
                sumB += weight * bPix;
                sumA += weight * aPix;
                sumW += weight;
            }
        }

        if (sumW <= 0.0f) {
            return RGB.rgba(0, 0, 0, 0);
        }
        float invW = 1.0f / sumW;
        return RGB.rgba(sumR * invW, sumG * invW, sumB * invW, sumA * invW);
    }

    /** 采样单个像素（世界坐标），若瓦片缺失返回透明 */
    private static float[] samplePixelTiled(Map<Long, float[]> tiles, int tileSize, int x, int y) {
        int tx = TiledCanvas.tileX(x, tileSize);
        int ty = TiledCanvas.tileY(y, tileSize);
        int localX = TiledCanvas.localX(x, tileSize);
        int localY = TiledCanvas.localY(y, tileSize);
        long key = TiledCanvas.pack(tx, ty);
        float[] tile = tiles.get(key);
        if (tile == null) {
            return RGB.rgba(0, 0, 0, 0);
        }
        int idx = 4 * (localY * tileSize + localX);
        return RGB.rgba(tile[idx], tile[idx+1], tile[idx+2], tile[idx+3]);
    }
}