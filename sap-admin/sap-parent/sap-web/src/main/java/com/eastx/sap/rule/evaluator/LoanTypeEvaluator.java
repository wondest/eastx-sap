package com.eastx.sap.rule.evaluator;

import com.eastx.sap.rule.data.LoanFact;
import com.eastx.sap.rule.data.Parameter;

/**
 * @ClassName LoanAmtEvaluator
 * @Description: User's
 * @Author Tender
 * @Time 2022/3/13 16:19
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class LoanTypeEvaluator extends AbstractEvaluator<LoanFact, String>{
    public LoanTypeEvaluator(Parameter parameter) {
        super(parameter, LoanFact::getLoanType);
    }
}
