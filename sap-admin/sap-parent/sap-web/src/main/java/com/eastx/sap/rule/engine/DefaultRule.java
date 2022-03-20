package com.eastx.sap.rule.engine;

import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.processor.Processor;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @ClassName definition
 * @Description:
 *  参考Drools,定义规则
 *
 *     When condition -- evaluator
 *     Then action -- processor
 *
 *  evaluator 设计：
 *      根据输入的事实和参数，进行真值判断
 *
 * @Author Tender
 * @Time 2022/3/13 8:35
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class DefaultRule<F> implements Rule<F> {
    /**
     * 规则编号(唯一)
     */
    private final String id;

    /**
     * 规则优先级
     */
    private final int priority;

    /**
     * 独占标识
     */
    private boolean exclusive;

    /**
     * 重复标识
     */
    private boolean repeat;

    /**
     * 规则求值器-Condition
     */
    private final Evaluator<F> evaluator;

    /**
     * 规则处理器-Action
     */
    private final Processor<F> processor;

    public DefaultRule(String id, Evaluator<F> evaluator, Processor<F> processor, int priority) {
        this.id = id;
        this.evaluator = evaluator;
        this.processor = processor;
        this.priority = priority;
        this.exclusive = true;
        this.repeat = false;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public int getPriority() {
        return priority;
    }

    public void setExclusive(boolean exclusive) {
        this.exclusive = exclusive;
    }

    @Override
    public boolean isExclusive() {
        return exclusive;
    }

    @Override
    public boolean isRepeat() {
        return repeat;
    }

    @Override
    public Function<F, Boolean> getCondition() {
        return (fact)->evaluator.execute(fact);
    }

    @Override
    public Consumer<F> getAction() {
        return (fact)->processor.execute(fact);
    }

    @Override
    public String toString() {
        return "SimpleRule{" +
                "id='" + id + '\'' +
                ", priority=" + priority +
                ", exclusive=" + exclusive +
                ", repeat=" + repeat +
                '}';
    }
}
