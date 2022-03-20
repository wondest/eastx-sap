package com.eastx.sap.rule.core.evaluator;

/**
 *
 */
public interface Evaluator<F> {
    /**
     * 根据事实进行求值
     *
     * @return
     */
    boolean execute(F fact);
}
