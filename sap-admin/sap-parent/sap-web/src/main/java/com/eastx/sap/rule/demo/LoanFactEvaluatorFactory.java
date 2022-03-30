package com.eastx.sap.rule.demo;

import com.eastx.sap.rule.core.evaluator.AbstractSimpleEvaluator;
import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.parameter.Parameter;

/**
 * @ClassName LoanFactProcessors
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 23:15
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class LoanFactEvaluatorFactory {
    /**
     *
     * @param parameter
     * @return
     */
    public static Evaluator getLoanAmt(Parameter<Integer>  parameter) {
        class LoanAmtEvaluator extends AbstractSimpleEvaluator<LoanFact, Integer> {

            public LoanAmtEvaluator(Parameter<Integer> parameter) {
                super("loanAmt", parameter, f->f.getLoanAmt());
            }
        }
        return new LoanAmtEvaluator(parameter);
    }

    /**
     *
     * @param parameter
     * @return
     */
    public static Evaluator getLoanType(Parameter<String> parameter) {
        class LoanTypeEvaluator extends AbstractSimpleEvaluator<LoanFact, String> {

            public LoanTypeEvaluator(Parameter<String> parameter) {
                super("loanType", parameter, f->f.getLoanType());
            }
        }
        return new LoanTypeEvaluator(parameter);
    }
}
