package com.eastx.sap.trader.strategy;

import com.eastx.sap.data.pojo.Price;
import com.eastx.sap.data.pojo.Signal;
import com.eastx.sap.data.pojo.Bar;
import com.eastx.sap.trader.core.SingleCerebra;
import com.eastx.sap.trader.core.Strategy;
import com.eastx.sap.trader.core.StrategySupport;
import com.eastx.sap.trader.core.buffer.LineSingle;
import com.eastx.sap.trader.core.data.DataFactory;
import com.eastx.sap.trader.core.data.LocalCsvDataFactory;
import com.eastx.sap.trader.core.data.MockExchangeFactory;
import com.eastx.sap.trader.core.data.SortedCacheDataFactory;
import com.eastx.sap.trader.core.data.feed.DataSeries;
import com.eastx.sap.trader.core.indicator.Indicator;
import com.eastx.sap.trader.core.indicator.DummySet;
import com.eastx.sap.trader.core.indicator.Lazy;
import lombok.extern.slf4j.Slf4j;
import java.util.function.Consumer;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @ClassName DummyStrategy
 * @Description: 直接获取OHLC数据
 * @Author Tender
 * @Time 2021/5/23 22:53
 * @Version 1.0
 * @Since 1.8
 **/
@Slf4j
public final class OhlcStrategy {
    /**
     *
     * @return
     */
    public static StrategySupport.MetaStrategy newInstance() {
        return new MyStrategy((e)->{});
    }

    /**
     *
     * @param observer
     * @return
     */
    public static StrategySupport.MetaStrategy newInstance(Consumer observer) {
        return new MyStrategy(observer);
    }

    /**
     *
     */
    private static class MyStrategy extends StrategySupport.MetaStrategy {
        /**
         * The minimum source of the strategy
         */
        private final int minSource = 1;

        /**
         * The minimum data(s) of the strategy (excludes the datetime)
         */
        private final int minData = 5;

        /**
         * The observer of the strategy
         */
        private Consumer observer;

        /**
         * The strategy will accept a observer
         * @param observer
         */
        MyStrategy(Consumer observer) {
            this.observer = checkNotNull(observer, "observer is null");
        }

        /**
         * Make  a strategy with the source(s)
         * @param sources
         * @return
         */
        @Override
        public Strategy doNew(DataSeries... sources) {
            checkNotNull(sources, "Strategy's sources should not be null");
            checkArgument(sources.length >= minSource, "This strategy needs at least %s source(s)", minSource);

            /**
             * The running strategy class.
             * @param minData
             * @param data
             */
            class RunningStrategy extends StrategySupport.BaseStrategy implements Strategy {
                /**
                 * A brief name for this Strategy, for logging purposes.
                 */
                private final String name = "Ohlc";

                /**
                 * 时间序列
                 */
                private final LineSingle datetime;

                /**
                 * 其他数据 open high low close
                 */
                private final LineSingle open;
                private final LineSingle high;
                private final LineSingle low;
                private final LineSingle close;
                private final LineSingle volume;

                /**
                 *
                 * @param minData
                 * @param data
                 * @param observer
                 */
                RunningStrategy(int minData, LineSingle datetime, LineSingle[] data, Consumer observer) {
                    super(observer);

                    //set data
                    checkNotNull(data, "Strategy's data should not be null.");
                    checkArgument(data.length >= minData, "This strategy needs at least %s data(s).", minData);

                    this.datetime = checkNotNull(datetime, "Strategy's datetime should not be null");
                    this.open = data[0];
                    this.high = data[1];
                    this.low = data[2];
                    this.close = data[3];
                    this.volume = data[4];

                    //set signal
                    setSignal(buildIndicator());
                }

                /**
                 * Build the indicator tree for the strategy
                 * @return
                 */
                private Indicator buildIndicator() {
                    Indicator datetime = new Lazy(this.datetime);
                    Indicator open = new Lazy(this.open);
                    Indicator high = new Lazy(this.high);
                    Indicator low = new Lazy(this.low);
                    Indicator close = new Lazy(this.close);
                    Indicator volume = new Lazy(this.volume);
                    Indicator dummy = new DummySet(datetime, new Indicator[] {open, high, low, close});

                    addIndicator(datetime);
                    addIndicator(open);
                    addIndicator(high);
                    addIndicator(low);
                    addIndicator(close);
                    addIndicator(volume);
                    addIndicator(dummy);

                    return dummy;
                }

                /**
                 * Get one bar of the current
                 */
                @Override
                public Bar getOneBar() {
                    String datetime = this.datetime.getBar().toString();

                    double open = this.open.getBar().doubleValue();
                    double high = this.high.getBar().doubleValue();
                    double low = this.low.getBar().doubleValue();
                    double close = this.close.getBar().doubleValue();
                    double volume = this.volume.getBar().doubleValue();

                    double signal = getSignal().getBar().doubleValue();

                    return Bar.valueOf(datetime, Signal.randOf(signal), Price.valueOf(open, high, low, close), volume);
                }

                /**
                 * Get one bar of the index
                 * @param index
                 * @return
                 */
                @Override
                protected Bar getOneBar(int index) {
                    String datetime = this.datetime.get(index).toString();

                    double open = this.open.get(index).doubleValue();
                    double high = this.high.get(index).doubleValue();
                    double low = this.low.get(index).doubleValue();
                    double close = this.close.get(index).doubleValue();
                    double volume = this.volume.get(index).doubleValue();

                    double signal = getSignal().get(index).doubleValue();

                    return Bar.valueOf(datetime, Signal.randOf(signal), Price.valueOf(open, high, low, close), volume);
                }
            }

            return new RunningStrategy(minData
                    ,sources[0].datetime()
                    ,new LineSingle[] {sources[0].open(), sources[0].high(), sources[0].low(), sources[0].close(), sources[0].volume()}
                    ,observer);
        }

        @Override
        public int minData() {
            return minData;
        }
    }

    /**
     * The test main
     *     the base path is the project home.
     * @param args
     */
    public static void main(String[] args) {
        DataFactory factory1 = new LocalCsvDataFactory("sap-trader/src/main/resources/trade_000001.SZ.csv");
        DataFactory factory2 = new MockExchangeFactory("sap-trader/src/main/resources/trade_000001.SZ.csv");
        DataFactory factory3 = new SortedCacheDataFactory("sap-trader/src/main/resources/trade_000001.SZ.csv");

        SingleCerebra.builder()
                .flow()
                .setFactory(factory2)
                .addStrategy(OhlcStrategy.newInstance())
                .build()
                .run();

        System.out.println("======= Mission Completed! =======");
    }
}
