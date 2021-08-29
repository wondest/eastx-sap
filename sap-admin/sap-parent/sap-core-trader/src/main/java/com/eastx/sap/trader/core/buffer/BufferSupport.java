package com.eastx.sap.trader.core.buffer;

/**
 * @ClassName BufferSupport
 * @Description: TODO
 * @Author Tender
 * @Time 2021/6/14 11:20
 * @Version 1.0
 * @Since 1.8
 **/
public final class BufferSupport {
    /**
     *
     * @return
     */
    public static LineSingle newLine() {
        return new LineBuffer();
    }

    /**
     *
     * @param name
     * @return
     */
    public static LineSingle newLine(String name) {
        return new LineBuffer(name);
    }
}
