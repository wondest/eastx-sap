package com.eastx.sap.rule.evaluator;

import com.eastx.sap.rule.data.Parameter;
import org.springframework.util.Assert;

import java.util.function.Function;

/**
 *
 *
 * @param <F> fact type
 * @param <T> value type
 */
public abstract class AbstractEvaluator<F, T> implements Evaluator<F> {
    /**
     * 预设参数
     */
    private Parameter<T> parameter;

    /**
     * 提取事实
     */
    private Function<F, T> getter;

    public AbstractEvaluator(Parameter parameter, Function<F, T> getter) {
        Assert.notNull(parameter, "The parameter should not be null");
        Assert.notNull(getter, "The getter should not be null");

        this.parameter = parameter;
        this.getter = getter;
    }

    @Override
    public boolean eval(F fact) {
        return parameter.check(getter.apply(fact));
    }
}
