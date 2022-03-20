package com.eastx.sap.rule.core.evaluator;

import org.springframework.util.Assert;

import java.util.Map;
import org.mvel2.MVEL;

/**
 * @ClassName MvelEvaluator
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/20 17:17
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class MvelEvaluator<F> implements Evaluator<F> {
    /**
     * 预设参数
     */
    private Map<String, Object> parameter;

    /**
     * 提取表达式
     */
    private String expression;

    public MvelEvaluator(String expression, Map<String, Object> parameter) {
        Assert.notNull(expression, "The expression should not be null");
        Assert.notNull(parameter, "The parameter should not be null");

        this.parameter = parameter;
        this.expression = expression;
    }

    @Override
    public boolean execute(F fact) {
        //如果需要带前缀,parameter.push("fact",fact),通过fact.xxx访问具体属性
        return (Boolean)MVEL.eval(expression, fact, parameter);
    }
}
