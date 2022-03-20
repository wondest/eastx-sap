package com.eastx.sap.rule.engine;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * 规则执行器
 *
 * @param <F>
 */
abstract class AbstractRuleExecutor<F> implements RuleExecutor<F> {
    @Override
    public boolean execute(Context<F> context, Rule<F> rule) {
        if (evalCondition(context, rule.getCondition())) {
            processAction(context, rule.getAction());
            return true;
        } else {
            return false;
        }
    }

    /**
     * 规则求值
     *
     * @param context
     * @param condition
     * @return
     */
    protected abstract boolean evalCondition(Context<F> context, Function<F, Boolean> condition);

    /**
     * 规则动作
     *
     * @param context
     * @param action
     */
    protected abstract void processAction(Context<F> context, Consumer<F> action);
}
