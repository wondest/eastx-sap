package com.eastx.sap.back.core.data.feed;

import com.eastx.sap.back.core.buffer.LineMultiple;
import com.eastx.sap.back.core.buffer.LineSingle;

/**
 * @ClassName DataSeries
 * @Description: TODO
 * @Author Tender
 * @Time 2021/5/26 21:34
 * @Version 1.0
 * @Since 1.8
 **/
public interface DataSeries extends LineMultiple {
    /**
     * 获取 datetime
     * @return
     */
    LineSingle datetime();

    /**
     * 获取 open price
     * @return
     */
    LineSingle open();

    /**
     * 获取 highest price
     * @return
     */
    LineSingle high();

    /**
     * 获取 close price
     * @return
     */
    LineSingle close();

    /**
     * 获取 lowest price
     * @return
     */
    LineSingle low();

    /**
     * 获取 成交量
     * @return
     */
    LineSingle volume();
}
