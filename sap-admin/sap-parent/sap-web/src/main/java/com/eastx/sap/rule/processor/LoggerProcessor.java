package com.eastx.sap.rule.processor;

/**
 *
 */
public class LoggerProcessor implements Processor {
    @Override
    public void execute() {
        System.out.print("Hello");
    }
}
