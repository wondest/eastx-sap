package com.eastx.sap.rule.engine;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * 默认的规则执行器
 *
 * @param <F>
 */
public class DefaultRuleExecutor<F> extends AbstractRuleExecutor<F> {
    @Override
    protected boolean evalCondition(Context<F> context, Function<F, Boolean> condition) {
        return condition.apply(context.getFact());
    }

    @Override
    protected void processAction(Context<F> context, Consumer<F> action) {
        action.accept(context.getFact());
    }
}
