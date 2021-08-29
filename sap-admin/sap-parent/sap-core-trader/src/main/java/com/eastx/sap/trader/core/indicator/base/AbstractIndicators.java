package com.eastx.sap.trader.core.indicator.base;

import com.eastx.sap.trader.core.buffer.LineSingle;
import com.eastx.sap.trader.core.indicator.Indicator;

import java.util.stream.IntStream;

/**
 * @ClassName Indicators
 * @Description: 继承自AbstractIndComposite,分为绑定类和非绑定类
 * @Author Tender
 * @Time 2021/5/23 16:12
 * @Version 1.0
 * @Since 1.8
 **/
public class AbstractIndicators {
    /**
     * 非绑定类指标
     */
    private abstract static class NoneBinding extends AbstractIndComposite {
        NoneBinding(String name, int period, LineSingle clock) {
            super(name, period, clock, false);
        }

        @Override
        public LineSingle proxyLine() {
            return null;
        }
    }

    /**
     * 僵尸类指标
     */
    public abstract static class Dummy extends AbstractIndComposite {
        public Dummy(String name, int period, LineSingle clock) {
            super(name, period, clock, false);
        }

        @Override
        protected void onceBefore(int startInclusive, int endExclusive) {
            dummyOperation();
        }

        @Override
        protected void onceFirst(int startInclusive, int endExclusive) {
            dummyOperation();
        }

        @Override
        protected void onceRemaining(int startInclusive, int endExclusive) {
            dummyOperation();
        }

        @Override
        protected void nextBefore() {
            dummyOperation();
        }

        @Override
        protected void nextFirst() {
            dummyOperation();
        }

        @Override
        protected void nextRemaining() {
            dummyOperation();
        }

        private void dummyOperation() {
            //do nothing
        }

        @Override
        public LineSingle proxyLine() {
            return null;
        }
    }

    /**
     * 绑定类指标
     */
    private abstract static class Binding extends AbstractIndComposite {
        Binding (String name, int period, LineSingle clock) {
            super(name, period, clock, true);
        }

        @Override
        protected void onceBefore(int startInclusive, int endExclusive) {
            dummyOperation();
        }

        @Override
        protected void onceFirst(int startInclusive, int endExclusive) {
            dummyOperation();
        }

        @Override
        protected void onceRemaining(int startInclusive, int endExclusive) {
            dummyOperation();
        }

        @Override
        protected void nextBefore() {
            dummyOperation();
        }

        @Override
        protected void nextFirst() {
            dummyOperation();
        }

        @Override
        protected void nextRemaining() {
            dummyOperation();
        }

        private void dummyOperation() {
            //do nothing
        }
    }

    /**
     *  计算类指标: 这里给出一部分默认实现，如果有需要则在子类中重写该方法
     */
    public abstract static class Eval extends NoneBinding {
        public Eval(String name, int period, LineSingle clock) {
            super(name, period, clock);
        }

        @Override
        protected void onceBefore(int startInclusive, int endExclusive) {
            //overwrite in child, if need
        }

        @Override
        protected void onceFirst(int startInclusive, int endExclusive) {
            doEvalOnce(startInclusive);
        }

        @Override
        protected void onceRemaining(int startInclusive, int endExclusive) {
            IntStream.range(startInclusive, endExclusive).forEach(this::doEvalOnce);
        }

        @Override
        protected void nextBefore() {
            //overwrite in child, if need
        }

        @Override
        protected void nextFirst() {
            //overwrite in child, if need
        }

        @Override
        protected void nextRemaining() {
            //overwrite in child, if need
        }

        /**
         * 计算当前指标
         *
         * @param i
         */
        protected abstract void doEvalOnce(int i);

        /**
         * 计算当前指标
         *
         */
        protected abstract void doEvalNext();
    }

    /**
     * 代理型指标: 代理指定的指标 或者直接代理数据
     */
    public abstract static class Proxy extends Binding {
        //Binding: line
        private LineSingle binding;

        public Proxy(String name, int period, Indicator indicator) {
            super(name, period, indicator);
            this.binding = addIndicator(indicator);
        }

        public Proxy(String name, int period, LineSingle data) {
            super(name, period, data);
            this.binding = data;
        }

        @Override
        public LineSingle proxyLine() {
            return binding;
        }
    }
}
