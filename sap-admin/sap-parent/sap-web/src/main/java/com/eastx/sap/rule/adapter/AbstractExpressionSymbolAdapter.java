package com.eastx.sap.rule.adapter;

/**
 * @ClassName adapter
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 20:58
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public abstract class AbstractExpressionSymbolAdapter implements ExpressionSymbolAdapter {
    @Override
    public String notEqual() {
        return "!=";
    }

    @Override
    public String equalTo() {
        return "==";
    }

    @Override
    public String greaterThan() {
        return ">";
    }

    @Override
    public String greaterOrEqual() {
        return ">=";
    }

    @Override
    public String lessThan() {
        return "<";
    }

    @Override
    public String lessOrEqual() {
        return "<=";
    }

    @Override
    public String space() {
        return " ";
    }
}
