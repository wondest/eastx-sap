package com.eastx.sap.rule.core.parameter;

import com.eastx.sap.rule.adapter.ExpressionSymbolAdapter;
import com.eastx.sap.rule.model.SetEnum;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;

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
     *
     */
    private String name;

    /**
     * Just for jackson
     */
    public SetParameter() {

    }

    public SetParameter(String name, SetEnum operation, Set<Object> set) {
        this.name = name;
        this.set = set;
        this.operation = operation;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
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
    public String getExpression(String fact, ExpressionSymbolAdapter adapter) {
        // (fact == xx1 or fact == xx2
        return switchAction(operation, adapter.variable(fact),
                // (fact == xx1 or fact == xx2)
                f->getIncludeExpression(f, adapter),
                // (fact != xx1 or fact != xx2)
                f->getExcludeExpression(f, adapter));
    }

    /**
     *
     * @param fact
     * @param emptyExpression
     * @param compareOperand
     * @param connectOperand
     * @return
     */
    private String getExpression(String fact, String emptyExpression, String compareOperand, String connectOperand) {
        if(set.isEmpty()) {
            return emptyExpression;
        }

        StringBuilder sb = new StringBuilder();

        int count = 0;
        for (Object o : set) {
            if(count == 0) {
                sb.append(fact).append(compareOperand).append("'").append(o.toString()).append("'");
            } else {
                sb.append(connectOperand).append(fact).append(compareOperand).append("'").append(o.toString()).append("'");;
            }

            count++;
        }

        return sb.toString();
    }

    /**
     *
     * @param fact
     * @param adapter
     * @return
     */
    private String getIncludeExpression(String fact, ExpressionSymbolAdapter adapter) {
        return getExpression(fact,
                new StringBuilder().append("1").append(adapter.equalTo()).append("0").toString(),
                adapter.equalTo(),
                new StringBuilder().append(adapter.space()).append(adapter.or()).append(adapter.space()).toString());
    }

    /**
     *
     * @param fact
     * @return
     */
    private String getExcludeExpression(String fact, ExpressionSymbolAdapter adapter) {
        return getExpression(fact,
                new StringBuilder().append("1").append(adapter.equalTo()).append("1").toString(),
                adapter.notEqual(),
                new StringBuilder().append(adapter.space()).append(adapter.and()).append(adapter.space()).toString());
    }

    /**
     *
     * @param type
     * @param value
     * @param includeAction
     * @param excludeAction
     * @param <R>
     * @param <S>
     * @return
     */
    private <R, S> R switchAction(SetEnum type, S value, Function<S, R> includeAction
            , Function<S, R> excludeAction) {
        if(SetEnum.INCLUDE.equals(type)) {
            return includeAction.apply(value);
        } else if(SetEnum.EXCLUDE.equals(type)) {
            return excludeAction.apply(value);
        } else {
            throw new UnsupportedOperationException(type.name());
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
