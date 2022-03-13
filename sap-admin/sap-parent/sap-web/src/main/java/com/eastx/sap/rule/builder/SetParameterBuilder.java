package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.SetEnum;
import com.eastx.sap.rule.data.Parameter;
import com.eastx.sap.rule.data.SetParameter;
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
    SetEnum operation;

    /**
     *
     */
    List<Object> elements = new LinkedList<>();

    /**
     *
     * @return
     */
    public SetParameterBuilder exclude() {
        this.operation = SetEnum.EXCLUDE;
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
        Assert.notNull(elements, "elements should not be null value");

        if(null == operation) {
            this.operation = SetEnum.INCLUDE;
        }

        return new SetParameter(elements.stream().collect(Collectors.toSet()), operation);
    }
}
