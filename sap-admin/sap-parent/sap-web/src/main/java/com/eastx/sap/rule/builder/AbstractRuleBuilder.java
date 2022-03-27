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
public abstract class AbstractRuleBuilder extends RuleBuilderHelper
        implements ConditionRuleBuilder, ActionRuleBuilder {

    @Override
    public ActionRuleBuilder processor(Processor processor) {
        return this;
    }

    @Override
    public ConditionRuleBuilder or(Evaluator operator) {
        concat(OperandEnum.OR).concat(operator);
        return this;
    }

    @Override
    public ActionRuleBuilder action(Processor processor) {
        setProcessor(processor);
        return this;
    }

    @Override
    public ActionRuleBuilder action() {
        return this;
    }

    /**
     * A java rule builder
     * @return
     */
    public JavaRuleBuilder java() {
        return new JavaRuleBuilder(this);
    }
}
