package com.eastx.sap.rule.core.evaluator;

import com.eastx.sap.rule.adapter.ExpressionSymbolAdapter;
import org.springframework.expression.Expression;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.util.Assert;

import java.util.Map;

/**
 * @ClassName MvelEvaluator
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/20 17:17
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class SpelEvaluator<F> implements Evaluator {
    /**
     * 预设参数
     */
    private Map<String, Object> parameter;

    /**
     * 提取表达式
     */
    private Expression expression;

    /**
     *
     */
    private String prefix;

    public SpelEvaluator(String expression, Map<String, Object> parameter) {
        this(expression, parameter, "fact");
    }

    public SpelEvaluator(String expression, Map<String, Object> parameter, String prefix) {
        Assert.notNull(expression, "The expression should not be null");
        Assert.notNull(parameter, "The parameter should not be null");
        Assert.hasText(prefix, "The factPrefix should not be empty");

        this.parameter = parameter;
        this.expression = new SpelExpressionParser().parseExpression(expression);
        this.prefix = prefix;
    }

    @Override
    public boolean execute(Object fact) {
        StandardEvaluationContext context = new StandardEvaluationContext();

        //设置参数
        context.setVariables(parameter);

        //设置事实(根路径)
        context.setRootObject(fact);

        //设置事实(如需要带前缀访问fact.xxx)
        //context.setVariable(prefix, fact);

        return (Boolean)expression.getValue(context);
    }

    @Override
    public String getExpression(ExpressionSymbolAdapter adapter) {
        return null;
    }
}
