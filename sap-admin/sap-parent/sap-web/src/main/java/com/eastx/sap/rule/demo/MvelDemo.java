package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.model.LoanFact;
import com.eastx.sap.rule.model.Parameter;
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
        Parameter parameter2 = ParameterBuilderFactory.get()
                .range("loanAmt")
                .between(Integer.valueOf(10), Integer.valueOf(60))
                .build();

        Parameter parameter1= ParameterBuilderFactory.get()
                .range("loanAmt")
                .lessThan(Integer.valueOf(60))
                .build();

        Parameter parameter3 = ParameterBuilderFactory.get()
                .set("loanType")
                .add("test")
                .add("test2")
                .build();

        System.out.println(parameter3.getMvel("loanType"));
        System.out.println(parameter3.getEntry());

        Boolean result = (Boolean)MVEL.eval(parameter3.getMvel("loanType"), fact);

        System.out.println(result);

    }
}
