package com.eastx.sap.back.core.data.feed;


/**
 * @ClassName DataFeed
 * @Description: Consume one bar,feed to DataSeries.
 * @Author Tender
 * @Time 2021/5/30 22:44
 * @Version 1.0
 * @Since 1.8
 **/
public interface DataFeed {
    /**
     * Get data
     * @return
     */
    DataSeries getData();

    /**
     * Feed data
     * @param oneBar
     * @return
     */
    boolean accept(Object oneBar);
}
