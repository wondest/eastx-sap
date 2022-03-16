package com.eastx.sap.rule.evaluator;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Supplier;

/**
 * 链式处理
 *
 *    (1) 链表方式构建，实现Evaluator接口
 *    (2) 实现：combine(self,next)
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
        return combine(evalSelf(fact), ()->evalNext(fact));
    }

    /**
     * or 操作的时候，如果第一个值是真，那么可以直接返回真值，实现快速运算
     * and 操作的时候，如果第一个值是假，那么可以直接返回假值，实现快速运算
     *
     * @param selfResult
     * @return
     */
    protected abstract boolean combine(boolean selfResult, Supplier<Boolean> nextResult);

    /**
     * eval next (maybe return null)
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
     *  a chain: a1->a2->a3
     *  b chain: b1->b2->b3
     *
     *  a.append(b) ->  a1->a2->a3->b(b1->b2->b3)
     * @param evaluator
     */
    public void append(AbstractChainedEvaluator evaluator) {
        this.tail.next = evaluator;
        this.tail = evaluator;
    }

    /**
     *  a chain: a1->a2->a3
     *  b chain: b1->b2->b3
     *
     *  a.append(b) ->  a1->a2->a3->b1->b2->b3
     * @param evaluator
     */
    public void extend(AbstractChainedEvaluator evaluator) {
        this.tail.next = evaluator;
        this.tail = evaluator.tail;
    }

    /**
     * The head
     *
     *   计算的时候，穿透直接使用next的真值
     *   如果next为空，则给默认的true(参考drools,没写条件默认位asset(true))
     * @param <F>
     */
    static class Head<F> extends AbstractChainedEvaluator<F> {
        @Override
        protected boolean combine(boolean selfResult, Supplier<Boolean> nextResult) {
            return Optional.ofNullable(nextResult.get()).orElse(true);
        }

        @Override
        protected boolean evalSelf(F fact) {
            return true;
        }
    }

    /**
     * The body
     *
     *    (1) combine the self and next using the combiner
     *    (2) 如果next为空，直接使用self真值
     *
     * @param <F>
     */
    private static class Body<F> extends AbstractChainedEvaluator<F> {
        /**
         * 代理处理
         */
        private Evaluator<F> delegate;

        /**
         * 真值合并
         */
        private BiFunction<Boolean, Supplier<Boolean>, Boolean> combiner;

        public Body(Evaluator<F> delegate, BiFunction<Boolean, Supplier<Boolean>, Boolean> combiner) {
            this.delegate = delegate;
            this.combiner = combiner;
        }

        @Override
        protected boolean combine(boolean selfResult, Supplier<Boolean> nextResult) {
            return combiner.apply(selfResult, nextResult);
        }

        @Override
        protected boolean evalSelf(F fact) {
            return delegate.eval(fact);
        }
    }

    /**
     * The self
     *    (1) combine is just use self
     * @param <F>
     */
    public static class BodySelf<F> extends Body<F> {
        public BodySelf(Evaluator<F> delegate) {
            super(delegate, (self, next)-> self);
        }
    }

    /**
     * The tail
     *    (1) combine is just use or
     * @param <F>
     */
    public static class BodyAnd<F> extends Body<F> {
        public BodyAnd(Evaluator<F> delegate) {
            super(delegate, (self, next)-> {
                if(!self) {
                    //短路操作
                    return false;
                }

                return Optional.ofNullable(next.get()).orElse(true);
            });
        }
    }

    /**
     * The tail
     *    (1) combine is just use or
     * @param <F>
     */
    public static class BodyOr<F> extends Body<F> {
        public BodyOr(Evaluator<F> delegate) {
            super(delegate, (self, next)-> {
                if(self) {
                    //短路操作
                    return true;
                }

                return Optional.ofNullable(next.get()).orElse(false);
            });
        }
    }
}
