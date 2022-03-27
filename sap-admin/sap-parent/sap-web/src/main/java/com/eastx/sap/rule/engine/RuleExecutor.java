package com.eastx.sap.rule.engine;

/**
 * @param <F>
 */
public interface RuleExecutor<F> {
    /**
     * 执行规则
     *
     * @return
     */
    boolean execute(Context<F> context, Rule rule);
}
