package com.eastx.sap.service;

import com.eastx.sap.data.pojo.Bar;
import com.eastx.sap.trader.core.SingleCerebra;
import com.eastx.sap.trader.core.data.DataFactory;
import com.eastx.sap.trader.core.data.MockExchangeFactory;
import com.eastx.sap.trader.core.data.SortedCacheDataFactory;
import com.eastx.sap.trader.strategy.OhlcStrategy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.Future;
import java.util.function.Consumer;

/**
 * @ClassName StockService
 * @Description: 提供Stock数据服务
 * @Author Tender
 * @Time 2021/7/11 0:01
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Service
public class StockService {
    @Async
    public Future<String> supplyFlow(Consumer begin, Consumer running, Consumer end) {
        DataFactory factory = new MockExchangeFactory("sap-trader/src/main/resources/trade_000001.SZ.csv");

        //supply a begin frame
        begin.accept(null);

        //supply data frame
        SingleCerebra.builder()
                .flow(true)
                .setFactory(factory)
                .addStrategy(OhlcStrategy.newInstance(slowConsumer(running, 100)))
                .build()
                .run();

        //supply a end frame
        end.accept(null);

        return new AsyncResult<>(Thread.currentThread().getName());
    }

    /**
     *
     * @return
     */
    public List<Bar> supplyOnce() {
        DataFactory factory = new SortedCacheDataFactory("sap-trader/src/main/resources/trade_000001.SZ.csv");

        //supply data frame
        SingleCerebra cerebra = SingleCerebra.builder()
                .flow(false)
                .setFactory(factory)
                .addStrategy(OhlcStrategy.newInstance())
                .build();

        cerebra.run();

        return cerebra.getImmediate();
    }

    /**
     * 防止WebSocket队列爆掉
     * @param old
     * @param millis
     * @return
     */
    private Consumer slowConsumer(Consumer old, long millis) {
        return t -> {
            try {
                Thread.sleep(millis);
            } catch (InterruptedException e) {
                e.printStackTrace();
            } finally {
                old.accept(t);
            }
        };
    }
}
