package com.eastx.sap.rule.data;

import com.eastx.sap.rule.core.SetEnum;

import java.util.Set;

/**
 * @ClassName ParameterPO
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 8:28
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class SetParameter implements Parameter {
    /**
     *
     */
    private Set<Object> set;

    /**
     *
     */
    private SetEnum operation;

    /**
     * Just for jackson
     */
    public SetParameter() {

    }

    public SetParameter(Set<Object> set, SetEnum operation) {
        this.set = set;
        this.operation = operation;
    }

    public Set<Object> getSet() {
        return set;
    }

    public void setSet(Set<Object> set) {
        this.set = set;
    }

    public SetEnum getOperation() {
        return operation;
    }

    public void setOperation(SetEnum operation) {
        this.operation = operation;
    }

    @Override
    public boolean check(Object value) {
        if(SetEnum.INCLUDE.equals(operation)) {
            return set.contains(value);
        } else if(SetEnum.EXCLUDE.equals(operation)) {
            return set.isEmpty() || !set.contains(value);
        }  else {
            return false;
        }
    }

    @Override
    public String toString() {
        return "SetParameter{" +
                "set=" + set +
                ", operation=" + operation +
                '}';
    }
}
