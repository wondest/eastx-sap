package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.adapter.MvelExpressionSymbolAdapter;
import com.eastx.sap.rule.adapter.SpelExpressionSymbolAdapter;
import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.parameter.Parameter;
import org.springframework.core.convert.TypeDescriptor;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import java.util.HashMap;
import java.util.Map;

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
        //1 fact,
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(49);

        //2 parameter,


        //3.
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
