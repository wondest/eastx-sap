package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.builder.RuleBuilderFactory;
import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.parameter.Parameter;
import com.eastx.sap.rule.core.processor.ThroughPassProcessor;
import com.eastx.sap.rule.engine.Rule;

/**
 * @ClassName ExpressionDemo
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 20:44
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class ExpressionDemo {

    public static void main(String[] argv) {

        Parameter parameter1 = ParameterBuilderFactory.get()
                .range("loanAmt")
                .between(Integer.valueOf(10), Integer.valueOf(60))
                .build();

        Parameter parameter2 = ParameterBuilderFactory.get()
                .set("loanType")
                .add("test")
                .add("test2")
                .build();


//        Rule rule = new RuleBuilderFactory().get("11")
//                .priority(10)
//                .condition(LoanFactEvaluators.getLoanAmt(parameter1))
//                .or(LoanFactEvaluators.getLoanType(parameter2))
//                .action().processor(new ThroughPassProcessor())
//                .java()
//                .build();

        Evaluator evaluator2 = LoanFactEvaluators.getLoanType(parameter2);

        System.out.println(evaluator2.getExpression(null));

    }
}
