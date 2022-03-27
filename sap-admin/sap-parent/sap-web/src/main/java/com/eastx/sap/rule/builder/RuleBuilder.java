package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.evaluator.Evaluator;

/**
 * @ClassName RuleBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 16:29
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class RuleBuilder extends AbstractRuleBuilder implements BaseRuleBuilder {

    RuleBuilder(String id) {
        setId(id);
    }

    @Override
    public ConditionRuleBuilder condition(Evaluator operator) {
        concat(operator);
        return this;
    }

    @Override
    public BaseRuleBuilder priority(int priority) {
        return this;
    }
}
