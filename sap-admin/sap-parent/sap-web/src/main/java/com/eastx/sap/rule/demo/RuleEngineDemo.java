package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.builder.EvaluatorBuilderFactory;
import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.core.evaluator.SpelEvaluator;
import com.eastx.sap.rule.engine.DefaultRule;
import com.eastx.sap.rule.model.LoanFact;
import com.eastx.sap.rule.model.Parameter;
import com.eastx.sap.rule.engine.*;
import com.eastx.sap.rule.core.evaluator.LoanAmtEvaluator;
import com.eastx.sap.rule.core.evaluator.LoanTypeEvaluator;
import com.eastx.sap.rule.core.evaluator.MvelEvaluator;
import com.eastx.sap.rule.core.processor.ThroughPassProcessor;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;

/**
 * @ClassName RuleEngineDemo
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/20 9:42
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
public class RuleEngineDemo {

    public static void main(String[] argv) {

        int maxRuns = 10000000;
        int loops = 3;

        double javaTime = IntStream.range(0, loops)
                .mapToLong(i->testJavaRule(maxRuns))
                .average().getAsDouble();
        log.debug(String.format("(java)合计批次：%d批，批次循环：%d次，平均耗时：%f ms", loops, maxRuns, javaTime));

        double spelTime = IntStream.range(0, loops)
                .mapToLong(i->testSpelRule(maxRuns))
                .average().getAsDouble();
        log.debug(String.format("(spel)合计批次：%d批，批次循环：%d次，平均耗时：%f ms", loops, maxRuns, spelTime));

        double mvelTime = IntStream.range(0, loops)
                .mapToLong(i->testSpelRule(maxRuns))
                .average().getAsDouble();

        log.debug(String.format("(mvel)合计批次：%d批，批次循环：%d次，平均耗时：%f ms", loops, maxRuns, mvelTime));
        log.debug(String.format("(java)合计批次：%d批，批次循环：%d次，平均耗时：%f ms", loops, maxRuns, javaTime));
        log.debug(String.format("(spel)合计批次：%d批，批次循环：%d次，平均耗时：%f ms", loops, maxRuns, spelTime));
    }

    private static long testSpelRule(int maxRuns) {
        //1 fact,
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(51);

        Context<LoanFact> context = new DefaultContext<>(fact);

        //2. engine
        DefaultRuleEngineFactory factory = new DefaultRuleEngineFactory();

        RuleEngine<LoanFact> engine = factory.getPerfEngine(maxRuns);

        //3. rule set
        RuleSet<Rule<LoanFact>> ruleSet = new DefaultRuleSet();

//        ruleSet.add(buildAcceptRule("rule11", 5));
//        ruleSet.add(buildAcceptRule("rule12", 6));
//        ruleSet.add(buildRejectRule("rule21", 7));
        //ruleSet.add(buildMvelRule("rule30", 99)); //循环：100000000次, 运行耗时 9615 ms
        ruleSet.add(buildSpelRule("rule30", 99)); //循环：100000000次, 运行耗时 9894 ms
        //ruleSet.add(buildJavaRule("rule30", 99)); //循环：100000000次, 运行耗时 9762 ms

        //4. assemble
        engine.assemble(ruleSet);

        //5. execute
        log.debug("S============================");
        long startTime = System.currentTimeMillis();
        engine.execute(context);
        long endTime = System.currentTimeMillis();
        log.debug("循环：" + maxRuns + "次, 运行耗时 " + (endTime-startTime) + " ms");
        log.debug("E============================");

        //6. print result
        //System.out.println(context);

        return endTime-startTime;
    }

    private static long testMvelRule(int maxRuns) {
        //1 fact,
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(51);

        Context<LoanFact> context = new DefaultContext<>(fact);

        //2. engine
        DefaultRuleEngineFactory factory = new DefaultRuleEngineFactory();

        RuleEngine<LoanFact> engine = factory.getPerfEngine(maxRuns);

        //3. rule set
        RuleSet<Rule<LoanFact>> ruleSet = new DefaultRuleSet();

//        ruleSet.add(buildAcceptRule("rule11", 5));
//        ruleSet.add(buildAcceptRule("rule12", 6));
//        ruleSet.add(buildRejectRule("rule21", 7));
        ruleSet.add(buildMvelRule("rule30", 99)); //循环：100000000次, 运行耗时 9615 ms
        //ruleSet.add(buildSpelRule("rule30", 99)); //循环：100000000次, 运行耗时 9894 ms
        //ruleSet.add(buildJavaRule("rule30", 99)); //循环：100000000次, 运行耗时 9762 ms

        //4. assemble
        engine.assemble(ruleSet);

        //5. execute
        log.debug("S============================");
        long startTime = System.currentTimeMillis();
        engine.execute(context);
        long endTime = System.currentTimeMillis();
        log.debug("循环：" + maxRuns + "次, 运行耗时 " + (endTime-startTime) + " ms");
        log.debug("E============================");

        //6. print result
        //System.out.println(context);

        return endTime-startTime;
    }

    private static long testJavaRule(int maxRuns) {
        //1 fact,
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(51);

        Context<LoanFact> context = new DefaultContext<>(fact);

        //2. engine
        DefaultRuleEngineFactory factory = new DefaultRuleEngineFactory();

        RuleEngine<LoanFact> engine = factory.getPerfEngine(maxRuns);

        //3. rule set
        RuleSet<Rule<LoanFact>> ruleSet = new DefaultRuleSet();

//        ruleSet.add(buildAcceptRule("rule11", 5));
//        ruleSet.add(buildAcceptRule("rule12", 6));
//        ruleSet.add(buildRejectRule("rule21", 7));
        //ruleSet.add(buildMvelRule("rule30", 99)); //循环：100000000次, 运行耗时 9615 ms
        //ruleSet.add(buildSpelRule("rule30", 99)); //循环：100000000次, 运行耗时 9894 ms
        ruleSet.add(buildJavaRule("rule30", 99)); //循环：100000000次, 运行耗时 9762 ms

        //4. assemble
        engine.assemble(ruleSet);

        //5. execute
        log.debug("S============================");
        long startTime = System.currentTimeMillis();
        engine.execute(context);
        long endTime = System.currentTimeMillis();
        log.debug("循环：" + maxRuns + "次, 运行耗时 " + (endTime-startTime) + " ms");
        log.debug("E============================");

        //6. print result
        //System.out.println(context);

        return endTime-startTime;
    }

    private static DefaultRule<LoanFact> buildSpelRule(String id, int priority) {
        //build parameters
        Map<String, Object> parameter = new HashMap<String, Object>();

        parameter.put("x1", 52);
        parameter.put("x2", 100);

        //build rule set
        DefaultRule<LoanFact> rule = new DefaultRule<LoanFact>(id,
                EvaluatorBuilderFactory.get().<LoanFact>stream(new SpelEvaluator<LoanFact>("#loanAmt>#x1 and #loanAmt<#x2", parameter))
                        .build()
                , new ThroughPassProcessor()
                ,priority);

        return rule;
    }

    private static DefaultRule<LoanFact> buildMvelRule(String id, int priority) {
        //build parameters
        Map<String, Object> parameter = new HashMap<String, Object>();

        parameter.put("x1", 52);
        parameter.put("x2", 100);

        //build rule set
        DefaultRule<LoanFact> rule = new DefaultRule<LoanFact>(id,
                EvaluatorBuilderFactory.get().<LoanFact>stream(new MvelEvaluator<LoanFact>("loanAmt>x1 && loanAmt<x2", parameter))
                        .build()
                , new ThroughPassProcessor()
                ,priority);

        return rule;
    }

    private static DefaultRule<LoanFact> buildJavaRule(String id, int priority) {
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
        DefaultRule<LoanFact> rule = new DefaultRule<LoanFact>(id,
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(parameter1))
                        .build()
                , new ThroughPassProcessor()
                ,priority);

        return rule;
    }

    private static DefaultRule<LoanFact> buildAcceptRule(String id, int priority) {
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
        DefaultRule<LoanFact> rule = new DefaultRule<LoanFact>(id,
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(parameter1))
                        .and(new LoanAmtEvaluator(parameter2))
                        .and(new LoanTypeEvaluator(parameter3))
                        .build()
                , new ThroughPassProcessor()
                ,priority);

        return rule;
    }

    private static DefaultRule<LoanFact> buildRejectRule(String id, int priority) {
        //build parameters
        Parameter parameter3 = ParameterBuilderFactory.get()
                .set("loanType")
                .add("test1")
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
        DefaultRule<LoanFact> rule = new DefaultRule<LoanFact>(id,
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(parameter1))
                        .and(new LoanAmtEvaluator(parameter2))
                        .and(new LoanTypeEvaluator(parameter3))
                        .build()
                , new ThroughPassProcessor()
                ,priority);
        return rule;
    }
}
