package com.eastx.sap.back.core.indicator;

import com.eastx.sap.back.core.buffer.LineSingle;
import com.eastx.sap.back.core.indicator.base.AbstractIndicators;

/**
 * @ClassName Lazy
 * @Description: TODO
 * @Author Tender
 * @Time 2021/7/11 0:41
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class Lazy extends AbstractIndicators.Proxy {
    public Lazy(int period, LineSingle data0) {
        super("Lazy_" + period, period, data0);
    }

    public Lazy(LineSingle data0) {
        this(1, data0);
    }
}
