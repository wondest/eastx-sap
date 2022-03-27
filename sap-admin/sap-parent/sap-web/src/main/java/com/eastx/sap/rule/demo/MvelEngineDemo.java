package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.core.parameter.Parameter;
import com.eastx.sap.rule.engine.*;
import com.eastx.sap.rule.factory.RuleEngineFactory;

/**
 * @ClassName RuleEngineDemo
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/20 9:42
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class MvelEngineDemo {

    public static void main(String[] argv) {
        //1 fact,
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(51);

        Context<LoanFact> context = new DefaultContext<>(fact);

        //2. engine
        RuleEngineFactory factory = new RuleEngineFactory();

        RuleEngine<LoanFact> engine = factory.getDefaultEngine();

        //3. rule set
        RuleSet ruleSet = new DefaultRuleSet();

        ruleSet.add(buildAcceptRule("rule11", 5));
        ruleSet.add(buildAcceptRule("rule12", 6));
        ruleSet.add(buildRejectRule("rule21", 7));
        ruleSet.add(buildRejectRule("rule30", 7));

        //4. assemble
        engine.assemble(ruleSet);

        //5. execute
        engine.execute(context);

        //6. print result
        System.out.println(context);
    }

    private static BeanRule buildAcceptRule(String id, int priority) {
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
//        DefaultRule<LoanFact> rule = new DefaultRule<LoanFact>(id,
//                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(parameter1))
//                        .and(new LoanAmtEvaluator(parameter2))
//                        .and(new LoanTypeEvaluator(parameter3))
//                        .build()
//                , new ThroughPassProcessor()
//                ,priority);

        return null;
    }

    private static BeanRule buildRejectRule(String id, int priority) {
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
//        DefaultRule<LoanFact> rule = new DefaultRule<LoanFact>(id,
//                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(parameter1))
//                        .and(new LoanAmtEvaluator(parameter2))
//                        .and(new LoanTypeEvaluator(parameter3))
//                        .build()
//                , new ThroughPassProcessor()
//                ,priority);
        return null;
    }
}
