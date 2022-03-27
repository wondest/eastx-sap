package com.eastx.sap.rule.core.parser;

import cn.hutool.core.lang.Assert;
import com.eastx.sap.rule.core.evaluator.Evaluator;

/**
 * @ClassName SPELExpressionParser
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 12:32
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class JavaParser implements ExpressionParser {
    @Override
    public ExpressionAdapter parseExpression(Object expression) {
        Assert.isInstanceOf(Evaluator.class, expression,"The expression should be a Evaluator");
        return new JavaExpression((Evaluator) expression);
    }
}
