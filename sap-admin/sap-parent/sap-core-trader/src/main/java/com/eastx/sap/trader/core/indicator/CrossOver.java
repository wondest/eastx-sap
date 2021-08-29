package com.eastx.sap.trader.core.indicator;

import com.eastx.sap.trader.core.buffer.BoxDouble;
import com.eastx.sap.trader.core.buffer.LineSingle;
import com.eastx.sap.trader.core.indicator.Indicator;
import com.eastx.sap.trader.core.indicator.base.BasicIndicators;
import com.eastx.sap.trader.core.indicator.base.AbstractIndicators;

/**
 * @ClassName CrossOver
 * @Description: 交叉指标计算，1表示上穿，-1表示下穿，0表示未交叉
 * @Author Tender
 * @Time 2021/5/23 16:38
 * @Version 1.0
 * @Since 1.8
 **/
public class CrossOver extends AbstractIndicators.Eval implements Indicator {
    /**
     * 输入的源数据
     */
    private final LineSingle data0;

    /**
     * 输入的源数据
     */
    private final LineSingle data1;

    public CrossOver(Indicator ind0, Indicator ind1) {
        super("CrossOver", Math.max(ind0.period(), ind1.period()), ind0);

        data0 = addIndicator(new CrossUp(ind0, ind1));
        data1 = addIndicator(new CrossDown(ind0, ind1));
    }

    @Override
    protected void doEvalOnce(int i) {
        set(i, data0.get(i).sub(data1.get(i)));
    }

    @Override
    protected void doEvalNext() {
        doEvalOnce(barIndex());
    }

    @Override
    protected void nextFirst() {
        doEvalNext();
    }

    @Override
    protected void nextRemaining() {
        doEvalNext();
    }

    /**
     * 基础的交叉指标 1-交叉 0-未交叉
     */
    private static class CrossBase extends AbstractIndicators.Eval {
        /**
         * 计算指标
         */
        private Indicator nzd;

        /**
         * 输入的源数据
         */
        private final LineSingle data0;

        /**
         * 输入的源数据
         */
        private final LineSingle data1;

        /**
         * 上穿标识
         */
        private boolean isCrossUp;

        CrossBase(int period, LineSingle data0, LineSingle data1, boolean isCrossUp) {
            super("CrossBase", period, data0);

            this.nzd = addIndicator(new BasicIndicators.NonZeroDifference(period, data0, data1));
            this.data0 = data0;
            this.data1 = data1;
            this.isCrossUp = isCrossUp;
        }

        @Override
        protected void onceFirst(int startInclusive, int endExclusive) {
            set(startInclusive, BoxDouble.ZERO);
        }

        @Override
        protected void doEvalOnce(int i) {
            boolean isBelow = false;
            boolean isAbove = false;

            //-1, 0, or 1 as this BigDecimal is numerically less than, equal to, or greater than val.
            int beforeCompared = nzd.get(i-1).compareTo(BoxDouble.ZERO);
            int afterCompared = data0.get(i).sub(data1.get(i)).compareTo(BoxDouble.ZERO);

            if(isCrossUp) {
                isBelow = (beforeCompared == -1);
                isAbove = (afterCompared == 1);
            } else {
                isAbove = (beforeCompared == 1);
                isBelow = (afterCompared == -1);
            }

            set(i, isBelow & isBelow?BoxDouble.ONE:BoxDouble.ZERO);
        }

        @Override
        protected void nextFirst() {
            setBar(BoxDouble.ZERO);
        }

        @Override
        protected void nextRemaining() {
            doEvalNext();
        }

        @Override
        protected void doEvalNext() {
            doEvalOnce(barIndex());
        }
    }

    /**
     * 上穿交叉指标 1-交叉 0-未交叉
     */
    private static class CrossUp extends AbstractIndicators.Proxy {
        CrossUp(int period, LineSingle data0, LineSingle data1) {
            super("CrossUp", period, new CrossBase(period, data0, data1, true));
        }

        CrossUp(Indicator ind0, Indicator ind1) {
            this(Math.max(ind0.period(), ind1.period()), ind0, ind1);
        }
    }

    /**
     * 下穿交叉指标 1-交叉 0-未交叉
     */
    private static class CrossDown extends AbstractIndicators.Proxy {
        CrossDown(int period, LineSingle data0, LineSingle data1) {
            super("CrossDown", period, new CrossBase(period, data0, data1, false));
        }

        CrossDown(Indicator ind0, Indicator ind1) {
            this(Math.max(ind0.period(), ind1.period()), ind0, ind1);
        }
    }
}
