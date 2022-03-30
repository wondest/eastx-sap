package com.eastx.sap.rule.core.parameter;

import com.eastx.sap.rule.adapter.ExpressionSymbolAdapter;
import com.eastx.sap.rule.model.RangeOperandEnum;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @ClassName RangeParameter
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
    private Range<T> range;

    /**
     *
     */
    private RangeOperandEnum operation;

    /**
     * Just for jackson
     */
    public RangeParameter() {
    }

    public RangeParameter(String name, RangeOperandEnum operation, T value, T lower, T upper) {
        this.range = new Range<>(value, lower, upper);
        this.operation = operation;
    }

    public Range<T> getRange() {
        return range;
    }

    public void setRange(Range<T> range) {
        this.range = range;
    }

    public RangeOperandEnum getOperation() {
        return operation;
    }

    public void setOperation(RangeOperandEnum operation) {
        this.operation = operation;
    }

    @Override
    public boolean check(T fact) {
        return switchAction(operation, fact,
                f->(f.compareTo(range.getLower()) >= 0) && (f.compareTo(range.getUpper()) <= 0),
                f->f.compareTo(range.getValue()) == 0,
                f->f.compareTo(range.getValue()) >= 0,
                f->f.compareTo(range.getValue()) > 0,
                f->f.compareTo(range.getValue()) <= 0,
                f->f.compareTo(range.getValue()) < 0);
    }

    @Override
    public String getExpression(String fact, ExpressionSymbolAdapter adapter) {
        return switchAction(operation, adapter.variable(fact),
                f->(new StringBuilder(f).append(adapter.greaterOrEqual()).append(range.getLower())
                        .append(adapter.space()).append(adapter.and()).append(adapter.space())
                        .append(f).append(adapter.lessOrEqual()).append(range.getUpper()).toString()),
                f->new StringBuilder(f).append(adapter.equalTo()).append(range.getValue()).toString(),
                f->new StringBuilder(f).append(adapter.greaterOrEqual()).append(range.getValue()).toString(),
                f->new StringBuilder(f).append(adapter.greaterThan()).append(range.getValue()).toString(),
                f->new StringBuilder(f).append(adapter.lessOrEqual()).append(range.getValue()).toString(),
                f->new StringBuilder(f).append(adapter.lessThan()).append(range.getValue()).toString());
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
    private <R, S> R switchAction(RangeOperandEnum type, S value, Function<S, R> betweenAction
            , Function<S, R> equalAction
            , Function<S, R> greaterEqualAction
            , Function<S, R> greaterThanAction
            , Function<S, R> lessEqualAction
            , Function<S, R> lessThanAction) {
        if(RangeOperandEnum.BW.equals(type)) {
            return betweenAction.apply(value);
        } else if(RangeOperandEnum.EQ.equals(type)) {
            return equalAction.apply(value);
        } else if(RangeOperandEnum.GE.equals(type)) {
            return greaterEqualAction.apply(value);
        } else if(RangeOperandEnum.GT.equals(type)) {
            return greaterThanAction.apply(value);
        } else if(RangeOperandEnum.LE.equals(type)) {
            return lessEqualAction.apply(value);
        } else if(RangeOperandEnum.LT.equals(type)) {
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
    private <R, S> R switchAction(RangeOperandEnum type, S value, Function<S, R> unaryAction, Function<S, R> binaryAction) {
        if(RangeOperandEnum.BW.equals(type)) {
            return binaryAction.apply(value);
        } else if(RangeOperandEnum.EQ.equals(type)) {
            return unaryAction.apply(value);
        } else if(RangeOperandEnum.GE.equals(type)) {
            return unaryAction.apply(value);
        } else if(RangeOperandEnum.GT.equals(type)) {
            return unaryAction.apply(value);
        } else if(RangeOperandEnum.LE.equals(type)) {
            return unaryAction.apply(value);
        } else if(RangeOperandEnum.LT.equals(type)) {
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
    private<R> void switchAction(RangeOperandEnum type, R value, Consumer<R> unaryAction, Consumer<R> binaryAction) {
        if(RangeOperandEnum.BW.equals(type)) {
            binaryAction.accept(value);
        } else if(RangeOperandEnum.EQ.equals(type)) {
            unaryAction.accept(value);
        } else if(RangeOperandEnum.GE.equals(type)) {
            unaryAction.accept(value);
        } else if(RangeOperandEnum.GT.equals(type)) {
            unaryAction.accept(value);
        } else if(RangeOperandEnum.LE.equals(type)) {
            unaryAction.accept(value);
        } else if(RangeOperandEnum.LT.equals(type)) {
            unaryAction.accept(value);
        } else {
            throw new UnsupportedOperationException(type.name());
        }
    }

    @Override
    public String toString() {
        return "SectionParameter{" +
                "range=" + range +
                ", operation=" + operation +
                '}';
    }

    private static class Range<T> {
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
         * for jackson
         */
        Range() {
        }

        public Range(T value, T lower, T upper) {
            this.value = value;
            this.lower = lower;
            this.upper = upper;
        }

        public T getLower() {
            return lower;
        }

        public void setLower(T lower) {
            this.lower = lower;
        }

        public T getUpper() {
            return upper;
        }

        public void setUpper(T upper) {
            this.upper = upper;
        }

        public T getValue() {
            return value;
        }

        public void setValue(T value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return "Range{" +
                    "lower=" + lower +
                    ", upper=" + upper +
                    ", value=" + value +
                    '}';
        }
    }
}
