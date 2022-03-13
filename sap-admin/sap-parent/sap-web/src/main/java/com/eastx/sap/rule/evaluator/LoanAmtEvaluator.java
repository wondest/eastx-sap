package com.eastx.sap.rule.evaluator;

import com.eastx.sap.rule.data.LoanFact;
import com.eastx.sap.rule.data.Parameter;

/**
 * @ClassName LoanAmtEvaluator
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 16:19
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class LoanAmtEvaluator extends AbstractEvaluator<LoanFact, Integer>{
    public LoanAmtEvaluator(Parameter parameter) {
        super(parameter, LoanFact::getLoanAmt);
    }
}
