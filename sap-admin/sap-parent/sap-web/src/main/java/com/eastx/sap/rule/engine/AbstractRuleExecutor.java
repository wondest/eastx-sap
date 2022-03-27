package com.eastx.sap.rule.engine;

import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * 规则执行器
 *
 * @param <F>
 */
abstract class AbstractRuleExecutor<F> implements RuleExecutor<F> {
    @Override
    public boolean execute(Context<F> context, Rule rule) {
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
    protected abstract boolean evalCondition(Context<F> context, Supplier condition);

    /**
     * 规则动作
     *
     * @param context
     * @param action
     */
    protected abstract void processAction(Context<F> context, Consumer action);
}
