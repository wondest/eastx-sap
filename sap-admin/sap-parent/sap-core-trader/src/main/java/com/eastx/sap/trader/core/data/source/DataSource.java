package com.eastx.sap.trader.core.data.source;

import java.util.function.Consumer;

/**
 * @ClassName DataSource
 * @Description: 数据源的基本操作
 * @Author Tender
 * @Time 2021/5/29 21:33
 * @Version 1.0
 * @Since 1.8
 **/
public interface DataSource {
    /**
     *
     * Feed all data once when the datasource is positive.
     *
     * @condition Positive
     * @param feed
     */
    void forAll(Consumer feed);

    /**
     * Start to post all data immediately.
     */
    void postAll();

    /**
     * Start to post a data one by one.
     *
     * @condition Event
     * @param oneBar
     */
    void post(Object oneBar);

    /**
     * Registry the subscriber.
     * within Google's EventBus using @Subscribe to marks a method as an event subscriber.
     *
     * @condition Event
     * @param subscriber
     *
     */
    void registry(Object subscriber);


    /**
     * Can use postAll or forAll.
     *
     * @return true when the datasource is positive.
     */
    boolean canAll();

    /**
     * The dataSource is passive or not.
     * @return
     */
    boolean isPassive();

    /**
     * Start action
     */
    void start();

    /**
     * Stop action
     */
    void stop();
}
