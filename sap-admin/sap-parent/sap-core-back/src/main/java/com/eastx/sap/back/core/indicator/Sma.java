package com.eastx.sap.back.core.indicator;

import com.eastx.sap.back.core.indicator.base.AbstractIndicators;
import com.eastx.sap.back.core.buffer.LineSingle;
import com.eastx.sap.back.core.indicator.base.BasicIndicators;

/**
 * @ClassName Sma
 * @Description: TODO
 * @Author Tender
 * @Time 2021/5/23 16:11
 * @Version 1.0
 * @Since 1.8
 **/
public class Sma extends AbstractIndicators.Proxy {
    /**
     * 输入的源数据
     */
    private final LineSingle data0;

    public Sma(int period, LineSingle data0) {
        super("Sma_" + period, period, new BasicIndicators.Average(period, data0));
        this.data0 = data0;
    }
}
