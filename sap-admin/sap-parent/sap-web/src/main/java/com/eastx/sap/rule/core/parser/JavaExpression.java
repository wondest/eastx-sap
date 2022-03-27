package com.eastx.sap.rule.core.parser;


import cn.hutool.core.lang.Assert;
import com.eastx.sap.rule.core.evaluator.Evaluator;

/**
 * @ClassName JavaExpression
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 20:40
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class JavaExpression implements ExpressionAdapter {
    /**
     * The expression object
     */
    private Evaluator expressionObject;

    public JavaExpression(Evaluator expression) {
        Assert.notNull(expression, "The expression should not be null");
        this.expressionObject = expression;
    }

    @Override
    public Boolean getValue(Object fact) {
        return expressionObject.execute(fact);
    }
}
