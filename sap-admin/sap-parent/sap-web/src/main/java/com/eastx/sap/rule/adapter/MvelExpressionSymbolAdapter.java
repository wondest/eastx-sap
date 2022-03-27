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
public class MvelExpressionSymbolAdapter extends AbstractExpressionSymbolAdapter {
    @Override
    public String and() {
        return "&&";
    }

    @Override
    public String or() {
        return "||";
    }

    @Override
    public String not() {
        return "!";
    }

    @Override
    public String variable(String name) {
        return new StringBuilder("fact.").append(name).toString();
    }
}
