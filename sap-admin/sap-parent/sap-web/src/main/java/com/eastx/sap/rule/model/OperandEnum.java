package com.eastx.sap.rule.model;

/**
 *
 */
public enum OperandEnum {
    AND(3),
    OR(2),
    NAN(1),
    END(0);

    OperandEnum(int priority) {
        this.priority = priority;
    }

    private final int priority;

    public int getPriority() {
        return priority;
    }
}
