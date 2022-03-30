package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.adapter.SpelExpressionSymbolAdapter;
import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.builder.RuleBuilderFactory;
import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.parameter.Parameter;
import com.eastx.sap.rule.core.processor.ThroughPassProcessor;
import com.eastx.sap.rule.engine.*;
import com.eastx.sap.rule.factory.RuleEngineFactory;

/**
 * @ClassName ExpressionDemo
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 20:44
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class JavaDemo {

    public static void main(String[] argv) {
        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(61);

        //build parameter - 数据库
        Parameter parameter1 = ParameterBuilderFactory.getObject()
                .range("loanAmt")
                .between(Integer.valueOf(10), Integer.valueOf(60))
                .build();

        Parameter parameter2 = ParameterBuilderFactory.getObject()
                .set("loanType")
                .add("test1")
                .add("test2")
                .build();


        Rule rule = new RuleBuilderFactory().rule("11")
                .priority(10)
                .condition(LoanFactEvaluatorFactory.getLoanAmt(parameter1).not())
                .or(LoanFactEvaluatorFactory.getLoanType(parameter2))
                .and(LoanFactEvaluatorFactory.getLoanType(parameter2))
                .or(LoanFactEvaluatorFactory.getLoanType(parameter2))
                .action().processor(new ThroughPassProcessor())
                .java()
                .build();

        System.out.println(((Evaluator) rule.getCondition().get()).getExpression(new SpelExpressionSymbolAdapter()));

        RuleSet ruleSet = new DefaultRuleSet();

        ruleSet.add(rule);

        RuleEngine ruleEngine = new RuleEngineFactory().java();

        ruleEngine.assemble(ruleSet);

        Context context = new DefaultContext(fact);

        ruleEngine.execute(context);

        System.out.println(context);

    }
}
