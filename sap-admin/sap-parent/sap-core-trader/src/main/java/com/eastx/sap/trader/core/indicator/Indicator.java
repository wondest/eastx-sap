package com.eastx.sap.trader.core.indicator;

import com.eastx.sap.trader.core.buffer.LineSingle;

/**
 * @ClassName Indicator
 * @Description: 指标类接口
 * @Author Tender
 * @Time 2021/5/23 15:57
 * @Version 1.0
 * @Since 1.8
 **/
public interface Indicator extends LineSingle {
    /**
     * Get the period of the indicator.
     * @return
     */
    int period();

    /**
     * Evaluate all once.
     */
    void evalOnce();

    /**
     * Evaluate one by one.
     */
    void evalNext();

    /**
     * Get the inner line in the indicator.
     * @return
     */
    LineSingle getLine();
}
