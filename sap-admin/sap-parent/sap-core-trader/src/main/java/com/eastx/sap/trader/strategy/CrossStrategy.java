package com.eastx.sap.trader.strategy;

import com.eastx.sap.data.pojo.Bar;
import com.eastx.sap.data.pojo.Price;
import com.eastx.sap.data.pojo.Signal;
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
import com.eastx.sap.trader.core.indicator.CrossOver;
import com.eastx.sap.trader.core.indicator.Sma;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @ClassName DemoStrategy
 * @Description: TODO
 * @Author Tender
 * @Time 2021/5/23 22:53
 * @Version 1.0
 * @Since 1.8
 **/
public class CrossStrategy {
    /**
     *
     * @return
     */
    public static StrategySupport.MetaStrategy newInstance() {
        return new CrossStrategy.MyStrategy();
    }

    /**
     *
     */
    public static class MyStrategy extends StrategySupport.MetaStrategy {
        /**
         * The minimum source of the strategy
         */
        private final int minSource = 1;

        /**
         * The minimum data(s) of the strategy (excludes the datetime)
         */
        private final int minData = 1;

        @Override
        public Strategy doNew(DataSeries... sources) {
            checkNotNull(sources, "Strategy's sources should not be null");
            checkArgument(sources.length >= minSource, "This strategy needs at least %s source(s)", minSource);

            class RunningStrategy extends StrategySupport.BaseStrategy implements Strategy {
                /**
                 * 时间序列
                 */
                private final LineSingle datetime;

                /**
                 * 其他数据 close
                 */
                private final LineSingle close;

                RunningStrategy(int minData, LineSingle datetime, LineSingle[] data) {
                    checkNotNull(data, "Strategy's data should not be null.");
                    checkArgument(data.length >= minData, "This strategy needs at least %s data(s).", minData);

                    this.datetime = checkNotNull(datetime, "Strategy's datetime should not be null");
                    this.close = data[0];

                    //set signal
                    setSignal(buildIndicator());
                }

                private Indicator buildIndicator() {
                    Indicator sma_fast = new Sma(10, this.close);
                    Indicator sma_slow = new Sma(15, this.close);
                    Indicator cross = new CrossOver(sma_fast, sma_slow);

                    addIndicator(sma_fast);
                    addIndicator(sma_slow);
                    addIndicator(cross);

                    return cross;
                }

                @Override
                protected Bar getOneBar(int index) {
                    String datetime = this.datetime.get(index).toString();
                    double close = this.close.getBar().doubleValue();
                    double signal = getSignal().get(index).doubleValue();

                    return Bar.valueOf(datetime, Signal.noneOf(signal), Price.valueOf(Double.NaN, Double.NaN, Double.NaN, close), Double.NaN);
                }

                @Override
                protected Bar getOneBar() {
                    String datetime = this.datetime.getBar().toString();
                    double close = this.close.getBar().doubleValue();
                    double signal = getSignal().getBar().doubleValue();

                    return Bar.valueOf(datetime, Signal.noneOf(signal), Price.valueOf(Double.NaN, Double.NaN, Double.NaN, close), Double.NaN);
                }
            }

            return new RunningStrategy(minData
                    ,sources[0].datetime()
                    ,new LineSingle[] {sources[0].close()});
        }

        @Override
        public int minData() {
            return minData;
        }
    }

    public static void main(String[] args) {
        DataFactory factory1 = new LocalCsvDataFactory("sap-trader/src/main/resources/trade_000001.SZ.csv");
        DataFactory factory2 = new MockExchangeFactory("src/main/resources/trade_000001.SZ.csv");
        DataFactory factory3 = new SortedCacheDataFactory("src/main/resources/trade_000001.SZ.csv");

        SingleCerebra.builder()
                .setFactory(factory3)
                .addStrategy(new MyStrategy())
                .build()
                .run();

        System.out.println("======= Mission Completed! =======");
    }
}
