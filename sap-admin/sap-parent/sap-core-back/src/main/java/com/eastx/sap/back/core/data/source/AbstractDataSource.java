package com.eastx.sap.back.core.data.source;

import com.google.common.eventbus.EventBus;

import java.util.function.Consumer;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @ClassName DataSource
 * @Description: 定义数据源的抽象行为
 * @Author Tender
 * @Time 2021/5/29 21:33
 * @Version 1.0
 * @Since 1.8
 **/
public abstract class AbstractDataSource implements DataSource {
    /**
     * 持久型的数据源,不允许发布数据
     *
     * 例如：本地文件数据源
     *
     * @function forAll
     */
    public abstract static class Persist extends AbstractDataSource {
        @Override
        public void postAll() {
            throw new UnsupportedOperationException("Persist's postAll is not supported");
        }

        @Override
        public void post(Object oneBar) {
            throw new UnsupportedOperationException("Persist's post is not supported");
        }

        @Override
        public void registry(Object subscriber) {
            throw new UnsupportedOperationException("Persist's registry is not supported");
        }

        @Override
        public boolean canAll() {
            return true;
        }

        @Override
        public boolean isPassive() {
            return false;
        }
    }

    /**
     * 发布型的数据源,允许注册订阅,主动发布数据
     *
     * @function post
     * @function registry
     */
    public abstract static class Publish extends AbstractDataSource {
        /**
         * A brief name for this dataSource, for logging purposes.
         */
        private String name;

        /**
         * A common publisher.
         */
        private EventBus eventBus;

        Publish(String name) {
            this.name = checkNotNull(name, "name shoud not be null");
            this.eventBus = new EventBus(name);
        }

        @Override
        public void post(Object oneBar) {
            eventBus.post(oneBar);
        }

        @Override
        public void registry(Object subscriber) {
            eventBus.register(subscriber);
        }

        @Override
        public boolean isPassive() {
            return false;
        }
    }

    /**
     * 代理型的数据源,代理一个数据源,让它可以发布数据
     *
     * @function post
     * @function registry
     * @function postAll
     * @function forAll
     */
    public abstract static class Proxy extends Publish {
        /**
         * Original dataSource
         */
        private DataSource dataSource;

        public Proxy(String name, DataSource dataSource) {
            super(name);
            this.dataSource = dataSource;
        }

        @Override
        public void forAll(Consumer feed) {
            dataSource.forAll(feed);
        }

        @Override
        public void postAll() {
            if(dataSource.isPassive()) {
                dataSource.postAll();
            } else {
                forAll(this::post);
            }
        }

        @Override
        public boolean canAll() {
            return dataSource.canAll();
        }

        @Override
        public boolean isPassive() {
            return true;
        }

        @Override
        public void start() {
            dataSource.start();
        }

        @Override
        public void stop() {
            dataSource.stop();
        }
    }
}
