package com.eastx.sap.rule.core.evaluator;

import com.eastx.sap.rule.adapter.ExpressionSymbolAdapter;

/**
 * Not xxx
 *
 * @param <F>
 */
public class ReverseEvaluatorWrapper<F> implements Evaluator {
    /**
     * 代理
     */
    private Evaluator delegate;

    public ReverseEvaluatorWrapper(Evaluator delegate) {
        this.delegate = delegate;
    }

    @Override
    public boolean execute(Object fact) {
        return !delegate.execute(fact);
    }

    @Override
    public String getExpression(ExpressionSymbolAdapter adapter) {
        return null;
    }
}
