package com.eastx.sap.rule.core.processor;

/**
 * 条件动作
 */
public interface Processor {
    /**
     * 执行动作
     */
    void execute(Object fact);
}
