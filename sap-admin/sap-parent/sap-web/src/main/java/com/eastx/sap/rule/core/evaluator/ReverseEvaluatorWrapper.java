package com.eastx.sap.rule.core.evaluator;

/**
 * Not xxx
 *
 * @param <F>
 */
public class ReverseEvaluatorWrapper<F> implements Evaluator<F> {
    /**
     *
     */
    private Evaluator<F> delegate;

    public ReverseEvaluatorWrapper(Evaluator<F> delegate) {
        this.delegate = delegate;
    }

    @Override
    public boolean execute(F fact) {
        return !delegate.execute(fact);
    }
}
