package com.eastx.sap.rule.core.evaluator;

import com.eastx.sap.rule.adapter.ExpressionSymbolAdapter;

/**
 * 求值器
 */
public interface Evaluator {
    /**
     * 根据事实进行求值
     *
     * @return
     */
    boolean execute(Object fact);

    /**
     * 获取求值表达式
     *
     * @param adapter  -- 语言适配
     * @return
     */
    String getExpression(ExpressionSymbolAdapter adapter);
}
