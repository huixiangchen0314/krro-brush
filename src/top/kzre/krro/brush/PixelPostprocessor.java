package top.kzre.krro.brush;

/**
 * 像素级后处理器，在混合完成后、写回画布前调用。
 * 实现可原地修改 pixel 数组（RGBA, float[4]）。
 */
@FunctionalInterface
public interface PixelPostprocessor {
    /**
     * @param pixel     当前 RGBA 像素 (混合结果)，可原地修改
     * @return 新像素
     */
    float[] process(float[] pixel);
}