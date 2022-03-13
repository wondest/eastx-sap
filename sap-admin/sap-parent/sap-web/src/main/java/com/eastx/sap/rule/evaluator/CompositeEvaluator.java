package com.eastx.sap.rule.evaluator;

/**
 * 复杂的计算
 *
 *  （1）andChain  and处理链
 *  （2）orChain   or处理链
 *
 * @param <F>
 */
public class CompositeEvaluator<F> implements EvaluatorStream<F>, Evaluator<F> {
    /**
     *  a && b && c
     */
    private AbstractChainedEvaluator andChain;

    /**
     *  a || b || c
     */
    private AbstractChainedEvaluator orChain;

    /**
     *
     */
    public CompositeEvaluator() {
        this.andChain = new AbstractChainedEvaluator.Head<>();
        this.orChain = new AbstractChainedEvaluator.Head<>();
    }

    @Override
    public boolean eval(F fact) {
        return andChain.eval(fact) && orChain.eval(fact);
    }

    @Override
    public EvaluatorStream<F> and(Evaluator<F> evaluator) {
        //append to the evaluator into andDelegate
        if(evaluator instanceof AbstractChainedEvaluator) {
            andChain.append((AbstractChainedEvaluator)evaluator);
        } else {
            andChain.append(wrapAnd(evaluator));
        }
        return this;
    }

    @Override
    public EvaluatorStream<F> or(Evaluator<F> evaluator) {
        //append to the evaluator into andDelegate
        if(evaluator instanceof AbstractChainedEvaluator) {
            orChain.append((AbstractChainedEvaluator)evaluator);
        } else {
            orChain.append(wrapOr(evaluator));
        }
        return this;
    }

    @Override
    public EvaluatorStream<F> andNot(Evaluator<F> evaluator) {
        //append to the evaluator into andDelegate
        andChain.append(wrapAnd(new ReverseEvaluatorWrapper<>(evaluator)));
        return this;
    }

    @Override
    public EvaluatorStream<F> orNot(Evaluator<F> evaluator) {
        orChain.append(wrapOr(new ReverseEvaluatorWrapper<>(evaluator)));
        return this;
    }

    @Override
    public Evaluator<F> getObject() {
        return this;
    }

    /**
     *
     * @param evaluator
     * @return
     */
    private AbstractChainedEvaluator wrapAnd(Evaluator<F> evaluator) {
        return new AbstractChainedEvaluator.WrapAnd(evaluator);
    }

    /**
     *
     * @param evaluator
     * @return
     */
    private AbstractChainedEvaluator wrapOr(Evaluator<F> evaluator) {
        return new AbstractChainedEvaluator.WrapOr(evaluator);
    }
}
