package com.eastx.sap.rule.core;

/**
 *
 */
public enum OperandEnum {
    AND(2),
    OR(1),
    NA(0),
    END(0);

    OperandEnum(int priority) {
        this.priority = priority;
    }

    private final int priority;

    public int getPriority() {
        return priority;
    }
}
