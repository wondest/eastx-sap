package com.eastx.sap.rule.core.evaluator;

import com.eastx.sap.rule.model.Parameter;
import org.springframework.util.Assert;

import java.util.function.Function;

/**
 *
 *
 * @param <F> the fact type
 * @param <V> the value type in fact
 */
public abstract class AbstractEvaluator<F, V> implements Evaluator<F> {
    /**
     * 预设参数
     */
    private Parameter<V> parameter;

    /**
     * 提取事实
     */
    private Function<F, V> getter;

    public AbstractEvaluator(Parameter<V> parameter, Function<F, V> getter) {
        Assert.notNull(parameter, "The parameter should not be null");
        Assert.notNull(getter, "The getter should not be null");

        this.parameter = parameter;
        this.getter = getter;
    }

    @Override
    public boolean execute(F fact) {
        return parameter.check(getter.apply(fact));
    }
}
