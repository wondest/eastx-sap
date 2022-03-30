package com.eastx.sap.rule.builder;

/**
 * @ClassName RuleBuilderFactory
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 23:41
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class RuleBuilderFactory {
    /**
     * When not in spring
     * @return
     */
    public static RuleBuilderFactory getObject() {
        return new RuleBuilderFactory();
    }

    /**
     *
     * @param id
     * @return
     */
    public BaseRuleBuilder rule(String id) {
        RuleBuilder builder = new RuleBuilder(id);
        return builder;
    }
}
