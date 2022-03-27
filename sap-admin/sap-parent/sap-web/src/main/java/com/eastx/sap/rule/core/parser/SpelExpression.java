package com.eastx.sap.rule.core.parser;

import cn.hutool.core.lang.Assert;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.Expression;
import org.springframework.expression.spel.support.SimpleEvaluationContext;
import org.springframework.expression.spel.support.StandardEvaluationContext;

/**
 * @ClassName StringExpression
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 20:40
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class SpelExpression implements ExpressionAdapter {
    /**
     * The expression object
     */
    private Expression expressionObject;

    public SpelExpression(Expression expression) {
        Assert.notNull(expression, "The expression should not be null");
        this.expressionObject = expression;
    }

    @Override
    public Boolean getValue(Object fact) {
        return (Boolean)expressionObject.getValue(withSimpleContext(fact));
    }

    /**
     *
     * @param fact
     * @return
     */
    private EvaluationContext withStandardContext(Object fact) {
        StandardEvaluationContext context = new StandardEvaluationContext();

        //设置事实
        context.setVariable("fact", fact);

        return context;
    }

    /**
     *
     * @param fact
     * @return
     */
    private EvaluationContext withSimpleContext(Object fact) {
        EvaluationContext context = SimpleEvaluationContext.forReadWriteDataBinding()
                .withRootObject(fact)
                .build();

        //设置事实
        context.setVariable("fact", fact);

        return context;
    }
}
