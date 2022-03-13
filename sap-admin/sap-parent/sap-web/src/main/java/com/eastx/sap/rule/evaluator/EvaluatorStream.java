package com.eastx.sap.rule.evaluator;

/**
 *
 * @param <F>
 */
public interface EvaluatorStream<F> {
    /**
     *
     * @param evaluator
     * @return
     */
    EvaluatorStream<F> and(Evaluator<F> evaluator);

    /**
     *
     * @param evaluator
     * @return
     */
    EvaluatorStream<F> or(Evaluator<F> evaluator);

    /**
     *
     * @param evaluator
     * @return
     */
    EvaluatorStream<F> andNot(Evaluator<F> evaluator);

    /**
     *
     * @param evaluator
     * @return
     */
    EvaluatorStream<F> orNot(Evaluator<F> evaluator);

    /**
     *
     * @return
     */
    Evaluator<F> getObject();
}
