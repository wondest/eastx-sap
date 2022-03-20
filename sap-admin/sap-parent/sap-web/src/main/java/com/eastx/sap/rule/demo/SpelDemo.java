package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.model.LoanFact;
import com.eastx.sap.rule.model.Parameter;
import org.mvel2.MVEL;
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

        Map<String, Object> parameter = new HashMap<>();

        parameter.put("x", 100);
        parameter.put("y", 50);

        //3.
        Parameter parameter2 = ParameterBuilderFactory.get()
                .range("loanAmt")
                .between(Integer.valueOf(10), Integer.valueOf(60))
                .build();

        Parameter parameter3 = ParameterBuilderFactory.get()
                .set("loanType")
                .exclude()
                .add("test1")
                .add("test2")
                .build();

        //3
        ExpressionParser parser = new SpelExpressionParser();

        System.out.println(parameter3.getSpel("loanType"));

        Expression expression = parser.parseExpression(parameter3.getSpel("loanType"));
        StandardEvaluationContext context = new StandardEvaluationContext();

        //context.setVariables(parameter2.getEntry());

        //引用属性不加前缀
        context.setRootObject(fact);

        //
        //context.setVariable("fact", fact);

        Boolean result = expression.getValue(context, Boolean.class);

        System.out.println(result);

    }
}
