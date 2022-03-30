package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.adapter.SpelExpressionSymbolAdapter;
import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.builder.RuleBuilderFactory;
import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.parameter.Parameter;
import com.eastx.sap.rule.core.processor.ThroughPassProcessor;
import com.eastx.sap.rule.engine.*;
import com.eastx.sap.rule.factory.RuleEngineFactory;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

/**
 * @ClassName MvelDemo
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/20 16:35
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class SpelDemo {
    public static void main(String[] argv) {

        testEngine();
    }

    public static void testEngine() {
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



        BeanRule rule2 = (BeanRule) new RuleBuilderFactory().rule("11")
                .priority(10)
                .condition(LoanFactEvaluatorFactory.getLoanAmt(parameter1))
                .or(LoanFactEvaluatorFactory.getLoanType(parameter2))
                .action().processor(new ThroughPassProcessor())
                .java()
                .build();

        Rule rule = new RuleBuilderFactory().rule("11")
                .priority(10)
                .condition(LoanFactEvaluatorFactory.getLoanAmt(parameter1).not())
                .or(LoanFactEvaluatorFactory.getLoanType(parameter2).not())
                //.or((Evaluator) rule2.getCondition().get())
                .action().processor(new ThroughPassProcessor())
                .spel()
                .build();

        System.out.println((rule.getCondition().get()));
        RuleSet ruleSet = new DefaultRuleSet();

        ruleSet.add(rule);

        RuleEngine ruleEngine = new RuleEngineFactory().spel();

        ruleEngine.assemble(ruleSet);

        Context context = new DefaultContext(fact);

        ruleEngine.execute(context);

        System.out.println(context);
    }

    public static void testSimple() {
        //1 fact,
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(49);

        //2 parameter,


        //3.
        Parameter parameter1 = ParameterBuilderFactory.getObject()
                .range("loanAmt")
                .between(Integer.valueOf(10), Integer.valueOf(60))
                .build();

        Parameter parameter2 = ParameterBuilderFactory.getObject()
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

        Evaluator evaluator2 = LoanFactEvaluatorFactory.getLoanType(parameter2);

        System.out.println(evaluator2.getExpression(new SpelExpressionSymbolAdapter()));

        //3
        ExpressionParser parser = new SpelExpressionParser();


        Expression expression = parser.parseExpression(evaluator2.getExpression(new SpelExpressionSymbolAdapter()));

        //Expression expression = parser.parseExpression("#fact.loanType=='test'");


        StandardEvaluationContext context = new StandardEvaluationContext();

        //context.setVariables(parameter2.getEntry());

        //引用属性不加前缀
        //context.setRootObject(fact);

        context.setVariable("fact", fact);

        //
        //context.setVariable("fact", fact);

        Boolean result = expression.getValue(context, Boolean.class);

        System.out.println(result);

    }
}
