package top.kzre.krro.brush;

/**
 * 不可变的值对象，表示一个输入事件（来自数位板、鼠标或其他指针设备）。
 * <p>
 * 包含所有常用物理和软件计算参数，与 Krita 的 KisPaintInformation 类保持概念一致。
 */
public final class PointerEvent {

    /**
     * 事件类型：下笔、移动、抬笔。
     */
    public enum EventType {
        DOWN,
        MOVE,
        UP
    }

    // 物理/辅助字段
    private final float x;                 // 画布坐标 X（子像素精度）
    private final float y;                 // 画布坐标 Y
    private final float pressure;          // 压力 [0, 1]，默认 0.5 或 1.0（鼠标）
    private final float tiltX;             // 水平倾斜角度（归一化或度数，取决于约定）
    private final float tiltY;             // 垂直倾斜角度
    private final float rotation;          // 笔身旋转角度
    private final long timestamp;          // 事件发生的时间戳（毫秒）
    private final float perspective;       // 透视辅助值（由软件计算，默认 0）
    private final EventType type;          // 事件类型

    // ---------- 私有构造函数 ----------
    private PointerEvent(float x, float y, float pressure,
                         float tiltX, float tiltY, float rotation,
                         long timestamp, float perspective,
                         EventType type) {
        this.x = x;
        this.y = y;
        this.pressure = pressure;
        this.tiltX = tiltX;
        this.tiltY = tiltY;
        this.rotation = rotation;
        this.timestamp = timestamp;
        this.perspective = perspective;
        this.type = type;
    }

    // ---------- 静态工厂方法 ----------

    /**
     * 完全构造函数，允许指定所有参数。
     */
    public static PointerEvent create(float x, float y, float pressure,
                                      float tiltX, float tiltY, float rotation,
                                      long timestamp, float perspective,
                                      EventType type) {
        return new PointerEvent(x, y, pressure, tiltX, tiltY, rotation,
                timestamp, perspective, type);
    }

    /**
     * 简化构造：只提供坐标、压力和类型，其余字段设为默认值。
     * 倾斜/旋转 = 0，perspective = 0，timestamp = 当前系统时间。
     */
    public static PointerEvent create(float x, float y, float pressure, EventType type) {
        return new PointerEvent(x, y, pressure,
                0f, 0f, 0f,
                System.currentTimeMillis(),
                0f, type);
    }

    /**
     * 更简化的构造：仅坐标和类型，压力使用默认值 0.5。
     */
    public static PointerEvent create(float x, float y, EventType type) {
        return create(x, y, 0.5f, type);
    }

    /**
     * 从鼠标事件构造（无压力感应），压力设为 1.0（代表完全按压）。
     * 倾斜/旋转=0，timestamp = now，perspective = 0。
     */
    public static PointerEvent fromMouse(float x, float y, EventType type) {
        return new PointerEvent(x, y, 1.0f,
                0f, 0f, 0f,
                System.currentTimeMillis(),
                0f, type);
    }

    // ---------- Getter ----------
    public float getX() { return x; }
    public float getY() { return y; }
    public float getPressure() { return pressure; }
    public float getTiltX() { return tiltX; }
    public float getTiltY() { return tiltY; }
    public float getRotation() { return rotation; }
    public long getTimestamp() { return timestamp; }
    public float getPerspective() { return perspective; }
    public EventType getType() { return type; }

    @Override
    public String toString() {
        return String.format("PointerEvent{x=%.2f, y=%.2f, pressure=%.2f, type=%s}",
                x, y, pressure, type);
    }

    // 为了在 Clojure 中方便使用，可以保留 equals/hashCode（基于字段）
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof PointerEvent)) return false;
        PointerEvent that = (PointerEvent) o;
        return Float.compare(that.x, x) == 0 &&
                Float.compare(that.y, y) == 0 &&
                Float.compare(that.pressure, pressure) == 0 &&
                Float.compare(that.tiltX, tiltX) == 0 &&
                Float.compare(that.tiltY, tiltY) == 0 &&
                Float.compare(that.rotation, rotation) == 0 &&
                timestamp == that.timestamp &&
                Float.compare(that.perspective, perspective) == 0 &&
                type == that.type;
    }

    @Override
    public int hashCode() {
        int result = Float.hashCode(x);
        result = 31 * result + Float.hashCode(y);
        result = 31 * result + Float.hashCode(pressure);
        result = 31 * result + Float.hashCode(tiltX);
        result = 31 * result + Float.hashCode(tiltY);
        result = 31 * result + Float.hashCode(rotation);
        result = 31 * result + Long.hashCode(timestamp);
        result = 31 * result + Float.hashCode(perspective);
        result = 31 * result + type.hashCode();
        return result;
    }
}