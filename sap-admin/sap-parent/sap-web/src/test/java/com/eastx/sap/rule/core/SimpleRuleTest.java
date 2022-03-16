package com.eastx.sap.rule.core;

import com.eastx.sap.rule.builder.EvaluatorBuilderFactory;
import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.data.LoanFact;
import com.eastx.sap.rule.data.Parameter;
import com.eastx.sap.rule.evaluator.LoanAmtEvaluator;
import com.eastx.sap.rule.evaluator.LoanTypeEvaluator;
import com.eastx.sap.rule.processor.ThroughPassProcessor;
import org.junit.jupiter.api.*;

class SimpleRuleTest {
    @BeforeAll
    public static void init() {
    }

    @AfterAll
    public static void clean() {
    }

    @BeforeEach
    void setUp() {
    }

    @AfterEach
    void tearDown() {
    }

    @DisplayName("testRejectAnd")
    @Test
    void testRejectAnd() {
        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(60);

        //build parameters
        Parameter paramTrue = ParameterBuilderFactory.get()
                .set()
                .add("test")
                .add("test2")
                .build();

        Parameter paramFalse1 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(1), Integer.valueOf(59))
                .build();

        Parameter paramTrue2 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(60), Integer.valueOf(100))
                .build();

        Parameter paramFalse3 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(61), Integer.valueOf(100))
                .build();

        //build rule set
        Rule<LoanFact> rule = new SimpleRule(
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(paramFalse1))
                        .and(new LoanAmtEvaluator(paramTrue2))
                        .and(new LoanTypeEvaluator(paramTrue))
                        .and(new LoanAmtEvaluator(paramFalse3))
                        .build()
                , new ThroughPassProcessor());

        //fire the rule
        rule.fire(fact);

        //check the result
        Assertions.assertFalse(fact.getAccept());
    }

    @DisplayName("testAcceptAnd")
    @Test
    void testAcceptAnd() {
        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(60);

        //build parameters
        Parameter paramTrue = ParameterBuilderFactory.get()
                .set()
                .add("test")
                .add("test2")
                .build();

        Parameter paramFalse1 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(1), Integer.valueOf(59))
                .build();

        Parameter paramTrue2 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(60), Integer.valueOf(100))
                .build();

        Parameter paramFalse3 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(61), Integer.valueOf(100))
                .build();

        //build rule set
        SimpleRule<LoanFact> rule = new SimpleRule(
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(paramTrue2))
                        .and(new LoanAmtEvaluator(paramTrue2))
                        .and(new LoanTypeEvaluator(paramTrue))
                        .and(new LoanAmtEvaluator(paramTrue2))
                        .build()
                , new ThroughPassProcessor());

        //fire the rule
        rule.fire(fact);

        //check the result
        Assertions.assertTrue(fact.getAccept());
    }

    @DisplayName("testRejectOr")
    @Test
    void testRejectOr() {
        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(60);

        //build parameters
        Parameter paramFalse = ParameterBuilderFactory.get()
                .set()
                .add("test1")
                .add("test2")
                .build();

        Parameter paramFalse1 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(1), Integer.valueOf(59))
                .build();

        Parameter paramTrue2 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(60), Integer.valueOf(100))
                .build();

        Parameter paramFalse3 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(61), Integer.valueOf(100))
                .build();

        //build rule set
        SimpleRule<LoanFact> rule = new SimpleRule(
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(paramFalse1))
                        .or(new LoanAmtEvaluator(paramFalse3))
                        .or(new LoanTypeEvaluator(paramFalse))
                        .or(new LoanAmtEvaluator(paramFalse3))
                        .build()
                , new ThroughPassProcessor());

        //fire the rule
        rule.fire(fact);

        //check the result(F or F or F or F)
        Assertions.assertFalse(fact.getAccept());
    }

    @DisplayName("testAcceptOr")
    @Test
    void testAcceptOr() {
        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(60);

        //build parameters
        Parameter paramTrue = ParameterBuilderFactory.get()
                .set()
                .add("test")
                .add("test2")
                .build();

        Parameter paramFalse1 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(1), Integer.valueOf(59))
                .build();

        Parameter paramTrue2 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(60), Integer.valueOf(100))
                .build();

        Parameter paramFalse3 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(61), Integer.valueOf(100))
                .build();

        //build rule set
        SimpleRule<LoanFact> rule = new SimpleRule(
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(paramFalse1))
                        .or(new LoanAmtEvaluator(paramTrue2))
                        .or(new LoanTypeEvaluator(paramTrue))
                        .or(new LoanAmtEvaluator(paramFalse1))
                        .build()
                , new ThroughPassProcessor());

        //fire the rule
        rule.fire(fact);

        //check the result(F or T or T or F)
        Assertions.assertTrue(fact.getAccept());
    }

    @DisplayName("testAcceptMix1")
    @Test
    void testAcceptMix1() {
        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(60);

        //build parameters
        Parameter paramTrue = ParameterBuilderFactory.get()
                .set()
                .add("test")
                .add("test2")
                .build();

        Parameter paramFalse1 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(1), Integer.valueOf(59))
                .build();

        Parameter paramTrue2 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(60), Integer.valueOf(100))
                .build();

        Parameter paramFalse3 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(61), Integer.valueOf(100))
                .build();

        //build rule set
        SimpleRule<LoanFact> rule = new SimpleRule(
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(paramFalse1))
                        .and(new LoanAmtEvaluator(paramTrue2))
                        .or(new LoanTypeEvaluator(paramTrue))
                        .and(new LoanAmtEvaluator(paramFalse1))
                        .build()
                , new ThroughPassProcessor());

        //fire the rule
        rule.fire(fact);

        //check the result ( F and T or T and F ) = F
        Assertions.assertFalse(fact.getAccept());
    }

    @DisplayName("testAcceptMix2")
    @Test
    void testAcceptMix2() {
        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(60);

        //build parameters
        Parameter paramTrue = ParameterBuilderFactory.get()
                .set()
                .add("test")
                .add("test2")
                .build();

        Parameter paramFalse1 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(1), Integer.valueOf(59))
                .build();

        Parameter paramTrue2 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(60), Integer.valueOf(100))
                .build();

        Parameter paramFalse3 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(61), Integer.valueOf(100))
                .build();

        //build rule set
        SimpleRule<LoanFact> rule = new SimpleRule(
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(paramTrue2))
                        .and(new LoanAmtEvaluator(paramTrue2))
                        .or(new LoanTypeEvaluator(paramTrue))
                        .and(new LoanAmtEvaluator(paramFalse1))
                        .build()
                , new ThroughPassProcessor());

        //fire the rule
        rule.fire(fact);

        //check the result ( T and T or T and F ) = F
        Assertions.assertFalse(fact.getAccept());
    }
}