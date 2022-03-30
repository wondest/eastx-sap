package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.processor.Processor;

/**
 * @ClassName ConditionRuleBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 9:11
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface ConditionRuleBuilder {
    /**
     * Set the or condition of a rule
     * @param operator
     * @return
     */
    ConditionRuleBuilder or(Evaluator operator);

    /**
     * Set the or condition of a rule
     * @param operator
     * @return
     */
    ConditionRuleBuilder and(Evaluator operator);

    /**
     * Next set the action of rule
     * @param processor
     * @return
     */
    ActionRuleBuilder action(Processor processor);

    /**
     * Next set the action of rule
     * @return
     */
    ActionRuleBuilder action();
}
