package com.eastx.sap.rule.core.parser;


import cn.hutool.core.lang.Assert;
import com.eastx.sap.rule.core.evaluator.Evaluator;
import org.mvel2.MVEL;

import java.util.HashMap;
import java.util.Map;

/**
 * @ClassName StringExpression
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 20:40
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class MvelExpression implements ExpressionAdapter {
    /**
     * The expression object
     */
    private String expressionObject;

    public MvelExpression(String expression) {
        Assert.notNull(expression, "The expression should not be null");
        this.expressionObject = expression;
    }

    @Override
    public Boolean getValue(Object fact) {
        Map<String, Object> variables = new HashMap<>(1);
        variables.put("fact", fact);
        return (Boolean)MVEL.eval(expressionObject, variables);
    }
}
