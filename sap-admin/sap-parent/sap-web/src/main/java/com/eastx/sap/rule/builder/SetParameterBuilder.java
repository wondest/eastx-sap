package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.model.SetOperandEnum;
import com.eastx.sap.rule.core.parameter.Parameter;
import com.eastx.sap.rule.core.parameter.SetParameter;
import org.springframework.util.Assert;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @ClassName ParameterBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 9:24
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class SetParameterBuilder {
    /**
     *
     */
    SetOperandEnum operation;

    /**
     *
     */
    List<Object> elements = new LinkedList<>();

    /**
     * 名称 - parameter name
     */
    private final String name;

    public SetParameterBuilder(String name) {
        this.name = name;
    }

    /**
     *
     * @return
     */
    public SetParameterBuilder exclude() {
        this.operation = SetOperandEnum.EXCLUDE;
        return this;
    }

    /**
     *
     * @param e
     * @return
     */
    public SetParameterBuilder add(List e) {
        elements.addAll(e);
        return this;
    }

    /**
     *
     * @param e
     * @return
     */
    public SetParameterBuilder add(Object e) {
        elements.add(e);
        return this;
    }

    /**
     *
     * @return
     */
    public Parameter build() {
        Assert.hasText(name, "name should not be empty");
        Assert.notNull(elements, "elements should not be null");

        if(null == operation) {
            this.operation = SetOperandEnum.INCLUDE;
        }

        return new SetParameter(name, operation, elements.stream().collect(Collectors.toSet()));
    }
}
