package com.eastx.sap.trader.core.indicator.base;

import com.eastx.sap.trader.core.buffer.BoxDouble;
import com.eastx.sap.trader.core.buffer.LineSingle;

/**
 * @ClassName BasicIndicators
 * @Description: 基础指标计算
 * @Author Tender
 * @Time 2021/5/23 16:20
 * @Version 1.0
 * @Since 1.8
 **/
public class BasicIndicators {
    /**
     * 计算移动平均指标
     */
    public static class Average extends AbstractIndicators.Eval {
        /**
         * 输入的源数据
         */
        private final LineSingle data0;

        public Average(int period, LineSingle data0) {
            super("Average_" + period, period, data0);
            this.data0 = data0;
        }

        @Override
        protected void nextFirst() {
            doEvalNext();
        }

        @Override
        protected void nextRemaining() {
            doEvalNext();
        }

        @Override
        protected void doEvalNext() {
            doEvalOnce(barIndex());
        }

        @Override
        protected void doEvalOnce(int i) {
            set(i, data0.slice(i-period()+1, i+1)
                      .reduce(BoxDouble.ZERO, BoxDouble::add)
                      .div(BoxDouble.valueOf(period())));
        }
    }

    /**
     * 计算非零差异指标
     */
    public static class NonZeroDifference extends AbstractIndicators.Eval {
        /**
         * 输入的源数据0
         */
        private final LineSingle data0;

        /**
         * 输入的源数据1
         */
        private final LineSingle data1;

        public NonZeroDifference(int period, LineSingle data0, LineSingle data1) {
            super("nzd", period, data0);
            this.data0 = data0;
            this.data1 = data1;
        }

        @Override
        protected void onceFirst(int startInclusive, int endExclusive) {
            this.set(startInclusive, data0.get(startInclusive).sub(data1.get(startInclusive)));
        }

        @Override
        protected void nextFirst() {
            setBar(data0.getBar().sub(data1.getBar()));
        }

        @Override
        protected void nextRemaining() {
            doEvalNext();
        }

        @Override
        protected void doEvalNext() {
            doEvalOnce(barIndex());
        }

        @Override
        protected void doEvalOnce(int i) {
            BoxDouble prev = get(i-1);
            BoxDouble diff = data0.get(i).sub(data1.get(i));
            set(i, diff.compareTo(BoxDouble.ZERO)==0?prev:diff);
        }
    }
}
