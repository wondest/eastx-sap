package com.eastx.sap.trader.core;

import com.eastx.sap.data.pojo.Bar;
import com.eastx.sap.trader.core.data.feed.DataSeries;
import com.eastx.sap.trader.core.indicator.Indicator;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.IntStream;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @ClassName Strategies
 * @Description: TODO
 * @Author Tender
 * @Time 2021/5/23 23:18
 * @Version 1.0
 * @Since 1.8
 **/
public abstract class StrategySupport {

    public static abstract class BaseStrategy {
        /**
         * 策略指标
         */
        private Indicator signal;

        /**
         * 策略子指标
         */
        private List<Indicator> indicators = new ArrayList<Indicator>();

        /**
         * 监听者
         */
        private Consumer observer;


        protected BaseStrategy(Consumer observer) {
            this.observer = checkNotNull(observer);
        }

        protected BaseStrategy() {
            this.observer = (a)->{/* do nothing */};
        }

        /**
         * 新增子指标
         * @param ind
         */
        protected void addIndicator(Indicator ind) {
            this.indicators.add(checkNotNull(ind));
        }

        /**
         * 设置策略指标
         * @param signal
         */
        protected void setSignal(Indicator signal) {
            this.signal = checkNotNull(signal);
        }

        /**
         * 获取策略指标
         */
        protected Indicator getSignal() {
            return this.signal;
        }

        /**
         * Get all bar of the strategy
         */
        public List<Bar> getAllBar() {
            List<Bar> result = new ArrayList<Bar>();
            IntStream.range(0, signal.size()).forEach(i->result.add(getOneBar(i)));
            return result;
        }

        /**
         * 获取指定索引值的Bar数据
         * @param index
         * @return
         */
        abstract protected Bar getOneBar(int index);

        /**
         * 获取当前索引值的Bar数据
         * @return
         */
        abstract protected Bar getOneBar();

        /**
         * 执行策略计算
         */
        public void evalOnce() {
            indicators.stream().forEach(ind -> ind.evalOnce());
            observer.accept(getAllBar());
        }

        /**
         * 执行策略计算
         */
        public void evalNext() {
            indicators.stream().forEach(ind -> ind.evalNext());
            observer.accept(getOneBar());
        }
    }

    /**
     * The meta class for a strategy.
     */
    public static abstract class MetaStrategy {
        /**
         * create a strategy.
         * @param data
         * @return
         */
        abstract public Strategy doNew(DataSeries...data);

        /**
         * The min number of the necessary datas for a strategy.
         * @return
         */
        abstract public int minData();
    }
}
