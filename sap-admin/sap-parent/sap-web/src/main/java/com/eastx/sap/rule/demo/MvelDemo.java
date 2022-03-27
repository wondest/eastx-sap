package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.adapter.MvelExpressionSymbolAdapter;
import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.parameter.Parameter;
import org.mvel2.MVEL;

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
public class MvelDemo {

    public static void main(String[] argv) {
        //1 fact,
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(49);

        //2 parameter,

        Map<String, Object> parameter = new HashMap<>();

        parameter.put("__loanAmtUpper", 100);
        parameter.put("__loanAmtLower", 50);
        parameter.put("__loanAmt", 90);

//        parameter.put("fact", fact);

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

        System.out.println(evaluator2.getExpression(new MvelExpressionSymbolAdapter()));

        Map<String, Object> varialbes = new HashMap<>(1);
        varialbes.put("fact", fact);

        Boolean result = (Boolean)MVEL.eval(evaluator2.getExpression(new MvelExpressionSymbolAdapter())
                , varialbes);

        System.out.println(result);

    }
}
