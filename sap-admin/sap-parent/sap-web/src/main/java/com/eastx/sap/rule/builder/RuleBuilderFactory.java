package com.eastx.sap.rule.builder;

import org.springframework.batch.core.step.builder.StepBuilder;

/**
 * @ClassName RuleBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 23:41
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class RuleBuilderFactory {

    /**
     *
     * @param id
     * @return
     */
    public BaseRuleBuilder get(String id) {
        RuleBuilder builder = new RuleBuilder(id);
        return builder;
    }

    public static void main(String[] argv) {

        new RuleBuilderFactory().get("11")
                .priority(10)
                .condition(null).or(null)
                .action().processor(null)
                .java().build();


        //factory.get(id)
        // .priority(10)
        // .condition(xxx).or(xxx).and(xxx)
        // .action(processor)
        // .java()
        // .build()

    }

}
