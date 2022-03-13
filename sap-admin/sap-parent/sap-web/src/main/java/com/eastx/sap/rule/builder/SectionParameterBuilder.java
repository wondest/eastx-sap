package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.SectionEnum;
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
public class SectionParameterBuilder<T extends Comparable> {
    /**
     * 区间类型
     */
    private SectionEnum operation;

    /**
     * 下限（如果是eq,那么 lower==upper)
     */
    private T lower;

    /**
     * 上限
     */
    private T upper;

    public SectionParameterBuilder<T> between(T lower, T upper) {
        operation = SectionEnum.BW;
        this.lower = lower;
        this.upper = upper;
        return this;
    }

    /**
     * 等于
     * @param value
     * @return
     */
    public SectionParameterBuilder<T> equal(T value) {
        return unary(SectionEnum.EQ, value);
    }

    /**
     *
     * @param value
     * @return
     */
    public SectionParameterBuilder<T> lessThan(T value) {
        return unary(SectionEnum.LT, value);
    }

    /**
     *
     * @param value
     * @return
     */
    public SectionParameterBuilder<T> lessEqual(T value) {
        return unary(SectionEnum.LE, value);
    }

    /**
     *
     * @param value
     * @return
     */
    public SectionParameterBuilder<T> greaterThan(T value) {
        return unary(SectionEnum.GT, value);
    }

    /**
     *
     * @param value
     * @return
     */
    public SectionParameterBuilder<T> greaterEqual(T value) {
        return unary(SectionEnum.GE, value);
    }

    /**
     *
     * @param operation
     * @param value
     * @return
     */
    private SectionParameterBuilder<T> unary(SectionEnum operation, T value) {
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
