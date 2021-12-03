package com.eastx.sap.back.core.buffer;

/**
 * 多线的基础接口
 * @author Tender
 */
public interface LineMultiple extends LineSeries<BoxDouble> {
    /**
     *  获取指定的线
     *
     * @param order
     * @return
     */
    public LineSingle getLine(int order);
}
