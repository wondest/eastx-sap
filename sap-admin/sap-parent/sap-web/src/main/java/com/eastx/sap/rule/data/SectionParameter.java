package com.eastx.sap.rule.data;

import com.eastx.sap.rule.core.SectionEnum;

/**
 * @ClassName ParameterPO
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 8:28
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class SectionParameter<T extends Comparable> implements Parameter<T>{

    T lower;

    T upper;

    SectionEnum operation;

    public SectionParameter(T lower, T upper, SectionEnum section) {
        this.upper = upper;
        this.lower = lower;
        this.operation = section;
    }

    public T getLoanAmtUpper() {
        return upper;
    }

    public void setLoanAmtUpper(T loanAmtUpper) {
        this.upper = loanAmtUpper;
    }

    public T getLower() {
        return lower;
    }

    public void setLower(T lower) {
        this.lower = lower;
    }

    public SectionEnum getOperation() {
        return operation;
    }

    public void setOperation(SectionEnum operation) {
        this.operation = operation;
    }

    @Override
    public boolean check(T value) {
        if(SectionEnum.BW.equals(operation)) {
            return (upper.compareTo(value) >= 0) && (lower.compareTo(value) <= 0);
        } else if(SectionEnum.EQ.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (lower.compareTo(value) == 0);
        } else if(SectionEnum.GE.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (upper.compareTo(value) <= 0);
        } else if(SectionEnum.GT.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (upper.compareTo(value) < 0);
        } else if(SectionEnum.LE.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (upper.compareTo(value) >= 0);
        } else if(SectionEnum.LT.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (upper.compareTo(value) > 0);
        } else {
            return false;
        }
    }
}
