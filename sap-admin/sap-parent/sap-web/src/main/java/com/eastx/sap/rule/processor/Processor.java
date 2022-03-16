package com.eastx.sap.rule.processor;

/**
 *
 */
public interface Processor<F> {
    /**
     *
     */
    void execute(F fact);
}
