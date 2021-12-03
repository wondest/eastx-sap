package com.eastx.sap.back.core.data;

import com.eastx.sap.back.core.data.feed.CsvDataFeed;
import com.eastx.sap.back.core.data.feed.DataFeed;
import com.eastx.sap.back.core.data.source.AbstractDataSource;
import com.eastx.sap.back.core.data.source.DataSource;
import com.eastx.sap.back.core.data.source.PreCachedDataSource;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @ClassName LocalCsvDataFactory
 * @Description: 以本地文件为数据源，模拟交易所事件驱动的数据源
 * @Author Tender
 * @Time 2021/6/13 22:33
 * @Version 1.0
 * @Since 1.8
 **/
public class MockExchangeFactory implements DataFactory {
    /**
     * 源文件全路径
     */
    private String fileName;

    public MockExchangeFactory(String fileName) {
        this.fileName = checkNotNull(fileName, "fileName should not be null");
    }

    @Override
    public DataFeed createFeed() {
        return new CsvDataFeed(1);
    }

    @Override
    public DataSource createSource() {
        class EventEnableSource extends AbstractDataSource.Proxy {
            EventEnableSource() {
                super("CsvEvent", new PreCachedDataSource(fileName, 1));
            }
        }

        return new EventEnableSource();
    }
}
