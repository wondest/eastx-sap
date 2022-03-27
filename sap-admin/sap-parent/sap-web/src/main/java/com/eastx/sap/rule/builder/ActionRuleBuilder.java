package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.processor.Processor;

/**
 * @ClassName ActionRuleBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 9:11
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface ActionRuleBuilder {
    /**
     * Set the processor of the rule
     * @param processor
     * @return
     */
    ActionRuleBuilder processor(Processor processor);

    /**
     * Next set the real builder
     * @return
     */
    JavaRuleBuilder java();
}
