package com.eastx.sap.rule.engine;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * 规则接口
 * @param <F> The fact type
 */
public interface Rule<F> extends DefaultRuleAdapter {
    /**
     * 条件求值
     * @return
     */
    Function<F, Boolean> getCondition();

    /**
     * 命中动作
     * @return
     */
    Consumer<F> getAction();
}
