package com.eastx.sap.rule.engine;

/**
 * 规则执行器
 */
public interface RuleExecutor {
    /**
     * 执行规则
     *
     * @return
     */
    boolean execute(Context context, Rule rule);
}
