package com.eastx.sap.rule.core.evaluator;

import com.eastx.sap.rule.adapter.ExpressionSymbolAdapter;

import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Supplier;

/**
 * 链式处理
 *
 *    (1) 链表方式构建，实现Evaluator接口
 *    (2) 实现：combine(self,next)
 */
public abstract class AbstractCompositeEvaluator implements Evaluator {
    /**
     * 责任链中下一个处理单元
     */
    private AbstractCompositeEvaluator next;

    /**
     * 责任链中下一个处理单元
     */
    private AbstractCompositeEvaluator tail;

    AbstractCompositeEvaluator() {
        this.tail = this;
        this.next = null;
    }

    @Override
    public boolean execute(Object fact) {
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
    private Boolean evalNext(Object fact) {
        if(null == next) {
            return null;
        } else {
            return next.execute(fact);
        }
    }

    /**
     * eval self
     *
     * @param fact
     * @return
     */
    protected abstract boolean evalSelf(Object fact);

    /**
     *  a chain: a1->a2->a3
     *  b chain: b1->b2->b3
     *
     *  a.append(b) ->  a1->a2->a3->b(b1->b2->b3)
     * @param evaluator
     */
    public void append(AbstractCompositeEvaluator evaluator) {
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
    public void extend(AbstractCompositeEvaluator evaluator) {
        this.tail.next = evaluator;
        this.tail = evaluator.tail;
    }


    /**
     * 获取求值表达式
     *
     * @param adapter  -- 语言适配
     * @return
     */
    @Override
    public String getExpression(ExpressionSymbolAdapter adapter) {
        StringBuilder expression = Optional.ofNullable(getSelfExpression(adapter))
                .map(s->new StringBuilder(s))
                .orElse(new StringBuilder());

        return Optional.ofNullable(next)
                .map(n->expression.append(n.getExpression(adapter)))
                .orElse(expression).toString();
    }

    abstract String getSelfExpression(ExpressionSymbolAdapter adapter);

    /**
     * The head
     *
     *   计算的时候，穿透直接使用next的真值
     *   如果next为空，则给默认的true(参考drools,没写条件默认位asset(true))
     */
    static class Head extends AbstractCompositeEvaluator {
        @Override
        protected boolean combine(boolean selfResult, Supplier<Boolean> nextResult) {
            return Optional.ofNullable(nextResult.get()).orElse(true);
        }

        @Override
        protected boolean evalSelf(Object fact) {
            return true;
        }

        @Override
        String getSelfExpression(ExpressionSymbolAdapter adapter) {
            return null;
        }
    }

    /**
     * The body
     *
     *    (1) combine the self and next using the combiner
     *    (2) 如果next为空，直接使用self真值
     */
    private static abstract class Body extends AbstractCompositeEvaluator {
        /**
         * 代理处理
         */
        private Evaluator delegate;

        /**
         * 真值合并
         */
        private BiFunction<Boolean, Supplier<Boolean>, Boolean> combiner;

        public Body(Evaluator delegate, BiFunction<Boolean, Supplier<Boolean>, Boolean> combiner) {
            this.delegate = delegate;
            this.combiner = combiner;
        }

        @Override
        protected boolean combine(boolean selfResult, Supplier<Boolean> nextResult) {
            return combiner.apply(selfResult, nextResult);
        }

        @Override
        protected boolean evalSelf(Object fact) {
            return delegate.execute(fact);
        }

        protected Evaluator getDelegate() {
            return delegate;
        }

        @Override
        protected String getSelfExpression(ExpressionSymbolAdapter adapter) {
            return getDelegate().getExpression(adapter);
        }
    }

    /**
     * self,next -> self
     */
    public static class BodySelf extends Body {
        public BodySelf(Evaluator delegate) {
            super(delegate, (self, next)-> self);
        }
    }

    /**
     * self,next -> self
     */
    public static class BodyAnd extends Body {
        public BodyAnd(Evaluator delegate) {
            super(delegate, (self, next)-> {
                if(!self) {
                    //短路操作
                    return false;
                }

                return Optional.ofNullable(next.get()).orElse(true);
            });
        }

        @Override
        protected String getSelfExpression(ExpressionSymbolAdapter adapter) {
            return new StringBuilder().append(getDelegate().getExpression(adapter))
                    .append("AND").toString();
        }
    }

    /**
     * self OR next ->
     */
    public static class BodyOr extends Body {
        public BodyOr(Evaluator delegate) {
            super(delegate, (self, next)-> {
                if(self) {
                    //短路操作
                    return true;
                }

                return Optional.ofNullable(next.get()).orElse(false);
            });
        }

        @Override
        protected String getSelfExpression(ExpressionSymbolAdapter adapter) {
            return new StringBuilder().append(getDelegate().getExpression(adapter))
                    .append("OR").toString();
        }
    }
}
