package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.RangeEnum;
import com.eastx.sap.rule.data.Parameter;
import com.eastx.sap.rule.data.SectionParameter;
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

    public RangeParameterBuilder<T> between(T lower, T upper) {
        this.operation = RangeEnum.BW;
        this.lower = lower;
        this.upper = upper;
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
        this.lower = value;
        this.upper = value;
        return this;
    }

    /**
     *
     * @return
     */
    public Parameter<T> build() {
        Assert.notNull(operation, "operation should be set with non-null value");
        Assert.notNull(lower, "lower should be set with non-null value");
        Assert.notNull(upper, "upper should be set with non-null value");

        return new SectionParameter<T>(lower, upper, operation);
    }
}
