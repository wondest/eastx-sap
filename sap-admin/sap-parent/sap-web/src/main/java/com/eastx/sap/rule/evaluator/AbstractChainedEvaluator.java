package com.eastx.sap.rule.evaluator;

import java.util.Optional;
import java.util.function.BiFunction;

/**
 * 链式处理
 *
 * @param <F>
 */
public abstract class AbstractChainedEvaluator<F> implements Evaluator<F> {
    /**
     * 责任链中下一个处理单元
     */
    private AbstractChainedEvaluator next;

    /**
     * 责任链中下一个处理单元
     */
    private AbstractChainedEvaluator tail;

    AbstractChainedEvaluator() {
        this.tail = this;
        this.next = null;
    }

    @Override
    public boolean eval(F fact) {
        return combined(evalSelf(fact), evalNext(fact));
    }

    /**
     * 计算结果合并
     *
     * @param selfResult
     * @param nextResult  maybe null
     * @return
     */
    protected abstract boolean combined(boolean selfResult, Boolean nextResult);

    /**
     * eval next
     *
     * @param fact
     * @return
     */
    private Boolean evalNext(F fact) {
        if(null == next) {
            return null;
        } else {
            return next.eval(fact);
        }
    }

    /**
     * eval self
     *
     * @param fact
     * @return
     */
    protected abstract boolean evalSelf(F fact);

    /**
     *
     * @param evaluator
     */
    protected void append(AbstractChainedEvaluator evaluator) {
        this.tail.next = evaluator;
        this.tail = evaluator;
    }

    /**
     * The head
     *   计算的时候，穿透直接使用next的真值
     *   如果条件为空，则给默认的true
     * @param <F>
     */
    static class Head<F> extends AbstractChainedEvaluator<F> {

        @Override
        protected boolean combined(boolean selfResult, Boolean nextResult) {
            return Optional.ofNullable(nextResult).orElse(true);
        }

        @Override
        protected boolean evalSelf(F fact) {
            return true;
        }
    }

    /**
     *
     * @param <F>
     */
    static class Wrap<F> extends AbstractChainedEvaluator<F> {
        /**
         * 代理处理
         */
        private Evaluator<F> delegate;

        /**
         * 真值合并
         */
        private BiFunction<Boolean, Boolean, Boolean> combiner;

        public Wrap(Evaluator<F> delegate, BiFunction<Boolean, Boolean, Boolean> combiner) {
            this.delegate = delegate;
            this.combiner = combiner;
        }

        @Override
        protected boolean combined(boolean selfResult, Boolean nextResult) {
            return Optional.ofNullable(nextResult)
                    .map(n->combiner.apply(selfResult, n))
                    .orElse(selfResult);
        }

        @Override
        protected boolean evalSelf(F fact) {
            return delegate.eval(fact);
        }
    }

    /**
     *
     * @param <F>
     */
    static class WrapAnd<F> extends Wrap<F> {
        public WrapAnd(Evaluator<F> delegate) {
            super(delegate, (a, b)->a && b);
        }
    }

    /**
     *
     * @param <F>
     */
    static class WrapOr<F> extends Wrap<F> {
        public WrapOr(Evaluator<F> delegate) {
            super(delegate, (a, b)->a || b);
        }
    }
}
