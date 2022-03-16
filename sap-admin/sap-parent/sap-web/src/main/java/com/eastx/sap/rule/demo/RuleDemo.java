package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.builder.EvaluatorBuilderFactory;
import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.core.SimpleRule;
import com.eastx.sap.rule.data.Parameter;
import com.eastx.sap.rule.data.LoanFact;
import com.eastx.sap.rule.data.SetParameter;
import com.eastx.sap.rule.evaluator.LoanAmtEvaluator;
import com.eastx.sap.rule.evaluator.LoanTypeEvaluator;
import com.eastx.sap.rule.processor.ThroughPassProcessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * @ClassName RuleMain
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 8:32
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class RuleDemo {


    /**
     *
     * 规则1：xxxx bw aaa and bbb
     * 规则2：yyyy gt aaa and bbb
     *
     * @param argv
     */
    public static void main(String[] argv) {
        System.out.println(" ======== start ========");

        testParameter();
    }


    private static void testParameter() {
        Parameter parameter3 = ParameterBuilderFactory.get()
                .set()
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
                .set()
                .add("test")
                .add("test2")
                .build();

        Parameter parameter1 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(50), Integer.valueOf(100))
                .build();

        Parameter parameter2 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(10), Integer.valueOf(60))
                .build();

        //build rule set
        SimpleRule rule = new SimpleRule(
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(parameter1))
                          .and(new LoanAmtEvaluator(parameter2))
                          .and(new LoanTypeEvaluator(parameter3))
                          .build()
                , new ThroughPassProcessor());

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
