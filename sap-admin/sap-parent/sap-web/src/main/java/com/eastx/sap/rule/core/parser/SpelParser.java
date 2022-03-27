package com.eastx.sap.rule.core.parser;

import cn.hutool.core.lang.Assert;
import org.springframework.expression.spel.standard.SpelExpressionParser;

/**
 * @ClassName SPELExpressionParser
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 12:32
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class SpelParser implements ExpressionParser {
    /**
     * 表达式解析器
     */
    private org.springframework.expression.ExpressionParser delegate = new SpelExpressionParser();

    @Override
    public ExpressionAdapter parseExpression(Object expression) {
        Assert.isInstanceOf(String.class, expression, "The expression should be a String");
        return new SpelExpression(delegate.parseExpression((String)expression));
    }
}
