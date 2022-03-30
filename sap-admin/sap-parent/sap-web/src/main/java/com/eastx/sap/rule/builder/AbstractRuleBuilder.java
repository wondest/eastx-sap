package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.processor.Processor;
import com.eastx.sap.rule.model.OperandEnum;

/**
 * @ClassName RuleBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 16:29
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public abstract class AbstractRuleBuilder extends RuleBuilderHelper<AbstractRuleBuilder>
        implements ConditionRuleBuilder, ActionRuleBuilder {

    @Override
    public ActionRuleBuilder processor(Processor processor) {
        setProcessor(processor);
        return this;
    }

    @Override
    public ConditionRuleBuilder or(Evaluator operator) {
        concat(OperandEnum.OR).concatReversed(operator);
        return this;
    }

    @Override
    public ConditionRuleBuilder and(Evaluator operator) {
        concat(OperandEnum.AND).concatReversed(operator);
        return this;
    }

    @Override
    public ActionRuleBuilder action(Processor processor) {
        setProcessor(processor);
        return this;
    }

    @Override
    public ActionRuleBuilder action() {
        concat(OperandEnum.END);
        return this;
    }

    /**
     *
     * @param evaluator
     * @return
     */
    protected AbstractRuleBuilder concatReversed(Evaluator evaluator) {
        if(evaluator.isReversed()) {
            concat(OperandEnum.NOT);
        }
        concat(evaluator);
        return this;
    }

    /**
     * A java rule builder
     * @return
     */
    public JavaRuleBuilder java() {
        return new JavaRuleBuilder(this);
    }

    /**
     * A spel rule builder
     * @return
     */
    public SpelRuleBuilder spel() {
        return new SpelRuleBuilder(this);
    }

    /**
     * A mvel rule builder
     * @return
     */
    public MvelRuleBuilder mvel() {
        return new MvelRuleBuilder(this);
    }
}
