package com.eastx.sap.rule.data;

/**
 * @ClassName Fact
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 8:29
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class LoanFact {
    /**
     * 贷款本金
     */
    private Integer loanAmt;

    /**
     * 还款方式
     */
    private String loanType;

    public Integer getLoanAmt() {
        return loanAmt;
    }

    public void setLoanAmt(Integer loanAmt) {
        this.loanAmt = loanAmt;
    }

    public String getLoanType() {
        return loanType;
    }

    public void setLoanType(String loanType) {
        this.loanType = loanType;
    }
}
