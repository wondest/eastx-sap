package com.eastx.sap.rule;

import com.eastx.sap.rule.builder.ParameterBuilder;
import com.eastx.sap.rule.core.SimpleRule;
import com.eastx.sap.rule.data.Parameter;
import com.eastx.sap.rule.evaluator.EvaluatorFactory;
import com.eastx.sap.rule.data.LoanFact;
import com.eastx.sap.rule.evaluator.LoanAmtEvaluator;
import com.eastx.sap.rule.evaluator.LoanTypeEvaluator;
import com.eastx.sap.rule.processor.LoggerProcessor;

/**
 * @ClassName RuleMain
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 8:32
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class RuleMain {


    /**
     *
     * 规则1：xxxx bw aaa and bbb
     * 规则2：yyyy gt aaa and bbb
     *
     * @param argv
     */
    public static void main(String[] argv) {
        System.out.println(" ======== start ========");

        testComposite();
    }

    private static void testComposite() {
        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(99);

        //build parameters
        Parameter parameter1 = ParameterBuilder.builder()
                .set()
                .add("test")
                .add("test2")
                .build();

        Parameter parameter2 = ParameterBuilder.builder()
                .section()
                .between(Integer.valueOf(10), Integer.valueOf(100))
                .build();

        //build rule set
        SimpleRule rule = new SimpleRule(
                  EvaluatorFactory.<LoanFact>stream()
                          .and(new LoanAmtEvaluator(parameter2))
                          .and(new LoanTypeEvaluator(parameter1))
                          .getObject()
                , new LoggerProcessor());

        //
        rule.fire(fact);
    }

//    private static void testSet() {
//        //build fact - 来自业务数据
//        LoanFact fact = new LoanFact();
//
//        fact.setLoanType("test");
//        fact.setLoanAmt(101);
//
//        //build parameter
//        Parameter parameter = ParameterBuilder.builder()
//                .set()
//                .exclude()
//                .add("test1")
//                .add("test2")
//                .build();
//
//        //build rule set
//        SimpleRule rule = new SimpleRule(new LoanTypeEvaluator(parameter), new LoggerProcessor());
//
//        //fire
//        rule.fire(fact);
//    }
//
//    private static void testSection() {
//        //build fact - 来自业务数据
//        LoanFact fact = new LoanFact();
//
//        fact.setLoanType("test");
//        fact.setLoanAmt(101);
//
//        //build parameter
//        Parameter parameter = ParameterBuilder.builder()
//                .section()
//                .between(Integer.valueOf(10), Integer.valueOf(100))
//                .build();
//
//        //build rule set
//        SimpleRule rule = new SimpleRule(new LoanAmtEvaluator(parameter), new LoggerProcessor());
//
//        //fire
//        rule.fire(fact);
//    }
}
