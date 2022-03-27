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
public class SpelExpressionSymbolAdapter extends AbstractExpressionSymbolAdapter {
    @Override
    public String and() {
        return "and";
    }

    @Override
    public String or() {
        return "or";
    }

    @Override
    public String not() {
        return "not";
    }

    @Override
    public String variable(String name) {
        return new StringBuilder("#fact.").append(name).toString();
    }
}
