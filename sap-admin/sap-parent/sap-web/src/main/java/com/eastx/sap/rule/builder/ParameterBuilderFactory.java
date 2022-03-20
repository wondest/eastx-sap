package com.eastx.sap.rule.builder;

/**
 * @ClassName ParameterBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 9:24
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class ParameterBuilderFactory {
    /**
     * 创建一个构建器
     *
     * @return
     */
    public static ParameterBuilderFactory get() {
        return new ParameterBuilderFactory();
    }

    /**
     * 构建集合参数
     *
     * @return
     */
    public SetParameterBuilder set(String name) {
        return new SetParameterBuilder(name);
    }

    /**
     * 构建区间参数
     * @return
     */
    public <T extends Comparable> RangeParameterBuilder range(String name) {
        return new RangeParameterBuilder<T>(name);
    }
}
