package com.eastx.sap.rule.core.evaluator;

import com.eastx.sap.rule.adapter.ExpressionSymbolAdapter;
import com.eastx.sap.rule.core.parameter.Parameter;
import org.springframework.util.Assert;

import java.util.function.Function;

/**
 * Parameter to check the fact's field
 *
 * @param <F> the fact type
 * @param <V> the fact value
 */
public abstract class AbstractSimpleEvaluator<F, V> implements Evaluator {
    /**
     * 预设参数
     */
    private Parameter<V> parameter;

    /**
     * 提取事实
     */
    private Function<F, V> getter;

    /**
     * 事实名称
     */
    private final String fact;

    public AbstractSimpleEvaluator(String fact, Parameter<V> parameter, Function<F, V> getter) {
        Assert.hasText(fact, "The fact should not be empty");
        Assert.notNull(parameter, "The parameter should not be null");
        Assert.notNull(getter, "The getter should not be null");

        this.fact = fact;
        this.parameter = parameter;
        this.getter = getter;
    }

    @Override
    public boolean execute(Object fact) {
        return parameter.check(getter.apply((F)fact));
    }

    /**
     * 获取求值表达式
     *
     * @param adapter  -- 语言适配
     * @return
     */
    @Override
    public String getExpression(ExpressionSymbolAdapter adapter) {
        return parameter.getExpression(fact, adapter);
    }
}
