package com.eastx.sap.rule.model;

import com.eastx.sap.rule.core.RangeEnum;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;
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
public class RangeParameter<T extends Comparable> implements Parameter<T>{
    /**
     * binary value
     */
    private T lower;

    /**
     * binary value
     */
    private T upper;

    /**
     * unary value
     */
    private T value;

    /**
     *
     */
    private RangeEnum operation;

    /**
     * Just for jackson
     */
    public RangeParameter() {

    }

    public RangeParameter(String name, RangeEnum operation, T value, T lower, T upper) {
        this.upper = upper;
        this.lower = lower;
        this.value = value;
        this.operation = operation;
    }

    public T getUpper() {
        return upper;
    }

    public void setUpper(T upper) {
        this.upper = upper;
    }

    public T getLower() {
        return lower;
    }

    public void setLower(T lower) {
        this.lower = lower;
    }

    public RangeEnum getOperation() {
        return operation;
    }

    public void setOperation(RangeEnum operation) {
        this.operation = operation;
    }

    @Override
    public boolean check(T fact) {
        return switchAction(operation, fact,
                f->(f.compareTo(lower) >= 0) && (f.compareTo(upper) <= 0),
                f->f.compareTo(value) == 0,
                f->f.compareTo(value) >= 0,
                f->f.compareTo(value) > 0,
                f->f.compareTo(value) <= 0,
                f->f.compareTo(value) < 0);
    }

    @Override
    public String getMvel(String fact) {
        return switchAction(operation, fact,
                f->(new StringBuilder(f).append(">=").append(lower).append(" && ")
                        .append(f).append("<=").append(upper).toString()),
                f->new StringBuilder(f).append("==").append(value).toString(),
                f->new StringBuilder(f).append(">=").append(value).toString(),
                f->new StringBuilder(f).append(">").append(value).toString(),
                f->new StringBuilder(f).append("<=").append(value).toString(),
                f->new StringBuilder(f).append("<").append(value).toString());
    }

    @Override
    public String getSpel(String fact) {
        return switchAction(operation, wrapSpelVarLabel(fact),
                f->new StringBuilder(f).append(">=").append(lower).append(" and ")
                        .append(f).append("<=").append(upper).toString(),
                f->new StringBuilder(f).append("==").append(value).toString(),
                f->new StringBuilder(f).append(">=").append(value).toString(),
                f->new StringBuilder(f).append(">").append(value).toString(),
                f->new StringBuilder(f).append("<=").append(value).toString(),
                f->new StringBuilder(f).append("<").append(value).toString());
    }

    /**
     * spel变量名是 #xxx
     * mvel变量名是 xxx
     *
     * @param name
     * @return
     */
    private String wrapSpelVarLabel(String name) {
        return new StringBuilder("#").append(name).toString();
    }

    /**
     *
     * @return
     */
    @Override
    public Map<String, Object> getEntry() {
        HashMap<String, Object> entry = new HashMap<>();

        return entry;
    }

    /**
     *
     * @param type
     * @param value
     * @param betweenAction
     * @param equalAction
     * @param greaterEqualAction
     * @param greaterThanAction
     * @param lessEqualAction
     * @param lessThanAction
     * @param <R>
     * @return
     */
    private <R, S> R switchAction(RangeEnum type, S value, Function<S, R> betweenAction
            , Function<S, R> equalAction
            , Function<S, R> greaterEqualAction
            , Function<S, R> greaterThanAction
            , Function<S, R> lessEqualAction
            , Function<S, R> lessThanAction) {
        if(RangeEnum.BW.equals(type)) {
            return betweenAction.apply(value);
        } else if(RangeEnum.EQ.equals(type)) {
            return equalAction.apply(value);
        } else if(RangeEnum.GE.equals(type)) {
            return greaterEqualAction.apply(value);
        } else if(RangeEnum.GT.equals(type)) {
            return greaterThanAction.apply(value);
        } else if(RangeEnum.LE.equals(type)) {
            return lessEqualAction.apply(value);
        } else if(RangeEnum.LT.equals(type)) {
            return lessThanAction.apply(value);
        } else {
            throw new UnsupportedOperationException(type.name());
        }
    }

    /**
     *
     * @param type
     * @param value
     * @param unaryAction
     * @param binaryAction
     * @param <R>
     * @return
     */
    private <R, S> R switchAction(RangeEnum type, S value, Function<S, R> unaryAction, Function<S, R> binaryAction) {
        if(RangeEnum.BW.equals(type)) {
            return binaryAction.apply(value);
        } else if(RangeEnum.EQ.equals(type)) {
            return unaryAction.apply(value);
        } else if(RangeEnum.GE.equals(type)) {
            return unaryAction.apply(value);
        } else if(RangeEnum.GT.equals(type)) {
            return unaryAction.apply(value);
        } else if(RangeEnum.LE.equals(type)) {
            return unaryAction.apply(value);
        } else if(RangeEnum.LT.equals(type)) {
            return unaryAction.apply(value);
        } else {
            throw new UnsupportedOperationException(type.name());
        }
    }

    /**
     *
     * @param type
     * @param value
     * @param unaryAction
     * @param binaryAction
     * @param <R>
     * @return
     */
    private<R> void switchAction(RangeEnum type, R value, Consumer<R> unaryAction, Consumer<R> binaryAction) {
        if(RangeEnum.BW.equals(type)) {
            binaryAction.accept(value);
        } else if(RangeEnum.EQ.equals(type)) {
            unaryAction.accept(value);
        } else if(RangeEnum.GE.equals(type)) {
            unaryAction.accept(value);
        } else if(RangeEnum.GT.equals(type)) {
            unaryAction.accept(value);
        } else if(RangeEnum.LE.equals(type)) {
            unaryAction.accept(value);
        } else if(RangeEnum.LT.equals(type)) {
            unaryAction.accept(value);
        } else {
            throw new UnsupportedOperationException(type.name());
        }
    }

    @Override
    public String toString() {
        return "SectionParameter{" +
                "lower=" + lower +
                ", upper=" + upper +
                ", operation=" + operation +
                '}';
    }
}
