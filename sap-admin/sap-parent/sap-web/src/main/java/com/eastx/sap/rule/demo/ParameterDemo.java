package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.builder.EvaluatorBuilderFactory;
import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.core.evaluator.LoanAmtEvaluator;
import com.eastx.sap.rule.core.evaluator.LoanTypeEvaluator;
import com.eastx.sap.rule.core.processor.ThroughPassProcessor;
import com.eastx.sap.rule.engine.DefaultRule;
import com.eastx.sap.rule.model.LoanFact;
import com.eastx.sap.rule.model.Parameter;
import com.eastx.sap.rule.model.SetParameter;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * @ClassName RuleMain
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 8:32
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class ParameterDemo {


    /**
     *
     * 规则1：xxxx bw aaa and bbb
     * 规则2：yyyy gt aaa and bbb
     *
     * @param argv
     */
    public static void main(String[] argv) {
        System.out.println(" ======== start ========");

        Parameter parameter2 = ParameterBuilderFactory.get()
                .range("loanAmt")
                .between(Integer.valueOf(10), Integer.valueOf(60))
                .build();

        System.out.println(parameter2.getMvel("factVale"));
        System.out.println(parameter2.getSpel("factVale"));
    }

    private static void testMvel() {

    }

    private static void testParameter() {
        Parameter parameter3 = ParameterBuilderFactory.get()
                .set("loanType")
                .add("test")
                .add("test2")
                .build();

        ObjectMapper mapper = new ObjectMapper();

        try {
            String value = mapper.writeValueAsString(parameter3);
            SetParameter object = mapper.readValue(value, SetParameter.class);

            System.out.println(value);
            System.out.println(object);
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }


    }

    private static void testComposite() {
        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(51);

        //build parameters
        Parameter parameter3 = ParameterBuilderFactory.get()
                .set("loanType")
                .add("test")
                .add("test2")
                .build();

        Parameter parameter1 = ParameterBuilderFactory.get()
                .range("loanAmt")
                .between(Integer.valueOf(50), Integer.valueOf(100))
                .build();

        Parameter parameter2 = ParameterBuilderFactory.get()
                .range("loanAmt")
                .between(Integer.valueOf(10), Integer.valueOf(60))
                .build();

        //build rule set
        DefaultRule rule = new DefaultRule("0",
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(parameter1))
                          .and(new LoanAmtEvaluator(parameter2))
                          .and(new LoanTypeEvaluator(parameter3))
                          .build()
                , new ThroughPassProcessor()
                ,1);

        //
//        rule.fire(fact);
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
