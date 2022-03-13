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
public class ParameterBuilder {
    /**
     * 创建一个构建器
     *
     * @return
     */
    public static ParameterBuilder builder() {
        return new ParameterBuilder();
    }

    /**
     * 构建集合参数
     *
     * @return
     */
    public SetParameterBuilder set() {
        return new SetParameterBuilder();
    }

    /**
     * 构建区间参数
     * @return
     */
    public <T extends Comparable> SectionParameterBuilder section() {
        return new SectionParameterBuilder<T>();
    }
}
