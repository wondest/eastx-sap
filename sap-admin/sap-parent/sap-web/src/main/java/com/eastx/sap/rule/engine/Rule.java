package com.eastx.sap.rule.engine;

import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * 规则接口
 */
public interface Rule extends ExtendRuleAdapter {
    /**
     * 条件求值
     * @return
     */
    Supplier getCondition();

    /**
     * 命中动作
     * @return
     */
    Consumer getAction();
}
