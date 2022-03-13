package com.eastx.sap.rule.evaluator;

/**
 *
 */
public interface Evaluator<F> {
    /**
     * 根据事实进行求值
     *
     * @return
     */
    boolean eval(F fact);
}
