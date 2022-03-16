package com.eastx.sap.rule.data;

import com.eastx.sap.rule.core.RangeEnum;

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

    RangeEnum operation;

    public SectionParameter(T lower, T upper, RangeEnum section) {
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

    public RangeEnum getOperation() {
        return operation;
    }

    public void setOperation(RangeEnum operation) {
        this.operation = operation;
    }

    @Override
    public boolean check(T value) {
        if(RangeEnum.BW.equals(operation)) {
            return (upper.compareTo(value) >= 0) && (lower.compareTo(value) <= 0);
        } else if(RangeEnum.EQ.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (lower.compareTo(value) == 0);
        } else if(RangeEnum.GE.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (upper.compareTo(value) <= 0);
        } else if(RangeEnum.GT.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (upper.compareTo(value) < 0);
        } else if(RangeEnum.LE.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (upper.compareTo(value) >= 0);
        } else if(RangeEnum.LT.equals(operation)) {
            return (lower.compareTo(upper) == 0) && (upper.compareTo(value) > 0);
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        return "SectionParameter{" +
                "lower=" + lower +
                ", upper=" + upper +
                ", operation=" + operation +
                '}';
    }
}
