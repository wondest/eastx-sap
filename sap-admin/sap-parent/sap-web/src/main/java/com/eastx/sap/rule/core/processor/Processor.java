package com.eastx.sap.rule.core.processor;

/**
 *
 */
public interface Processor<F> {
    /**
     *
     */
    void execute(F fact);
}
