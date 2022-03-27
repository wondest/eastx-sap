package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.evaluator.Evaluator;

/**
 * @ClassName BaseRuleBuilder
 * @Description:
 * @Author Tender
 * @Time 2022/3/27 16:29
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface BaseRuleBuilder {
    /**
     * Set priority of rule
     *
     * @param priority
     * @return
     */
    BaseRuleBuilder priority(int priority);

    /**
     * Next set the condition of rule
     * @param operator
     * @return
     */
    ConditionRuleBuilder condition(Evaluator operator);
}
