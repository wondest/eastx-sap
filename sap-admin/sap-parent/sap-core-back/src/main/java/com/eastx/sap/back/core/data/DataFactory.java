package com.eastx.sap.back.core.data;

import com.eastx.sap.back.core.data.feed.DataFeed;
import com.eastx.sap.back.core.data.source.DataSource;

/**
 * @ClassName DataFactory
 * @Description: 数据工厂类, feed 和 source 使用了泛型，实际运行时候会进行类型擦除，在转换的时候要能够匹配上
 * @Author Tender
 * @Time 2021/6/13 22:22
 * @Version 1.0
 * @Since 1.8
 **/
public interface DataFactory {
    /**
     * 创建一个数据
     * @return
     */
    DataFeed createFeed();

    /**
     * 创建一个数据源
     * @return
     */
    DataSource createSource();
}
