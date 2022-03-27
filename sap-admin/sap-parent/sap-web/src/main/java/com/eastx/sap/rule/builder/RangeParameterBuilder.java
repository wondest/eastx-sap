package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.model.RangeEnum;
import com.eastx.sap.rule.core.parameter.Parameter;
import com.eastx.sap.rule.core.parameter.RangeParameter;
import org.springframework.util.Assert;

/**
 * @ClassName SectionParameterBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 9:24
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class RangeParameterBuilder<T extends Comparable> {
    /**
     * 区间类型
     */
    private RangeEnum operation;

    /**
     * 下限（如果是eq,那么 lower==upper)
     */
    private T lower;

    /**
     * 上限
     */
    private T upper;

    /**
     * 当前值
     */
    private T value;

    /**
     * 名称 - parameter name
     */
    private final String name;

    public RangeParameterBuilder(String name) {
        this.name = name;
    }

    public RangeParameterBuilder<T> between(T lower, T upper) {
        binary(RangeEnum.BW, lower, upper);
        return this;
    }

    /**
     * 等于
     * @param value
     * @return
     */
    public RangeParameterBuilder<T> equal(T value) {
        return unary(RangeEnum.EQ, value);
    }

    /**
     *
     * @param value
     * @return
     */
    public RangeParameterBuilder<T> lessThan(T value) {
        return unary(RangeEnum.LT, value);
    }

    /**
     *
     * @param value
     * @return
     */
    public RangeParameterBuilder<T> lessEqual(T value) {
        return unary(RangeEnum.LE, value);
    }

    /**
     *
     * @param value
     * @return
     */
    public RangeParameterBuilder<T> greaterThan(T value) {
        return unary(RangeEnum.GT, value);
    }

    /**
     *
     * @param value
     * @return
     */
    public RangeParameterBuilder<T> greaterEqual(T value) {
        return unary(RangeEnum.GE, value);
    }

    /**
     *
     * @param operation
     * @param value
     * @return
     */
    private RangeParameterBuilder<T> unary(RangeEnum operation, T value) {
        this.operation = operation;
        this.lower = null;
        this.upper = null;
        this.value = value;
        return this;
    }

    /**
     *
     * @param operation
     * @param lower
     * @param upper
     * @return
     */
    private RangeParameterBuilder<T> binary(RangeEnum operation, T lower, T upper) {
        this.operation = operation;
        this.lower = lower;
        this.upper = upper;
        this.value = null;
        return this;
    }

    /**
     *
     * @return
     */
    public Parameter<T> build() {
        Assert.hasText(name, "name should not be empty");
        Assert.notNull(operation, "operation should not be null");

        return new RangeParameter<T>(name, operation, value, lower, upper);
    }
}
