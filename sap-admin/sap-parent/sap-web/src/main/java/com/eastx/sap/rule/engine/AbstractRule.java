package com.eastx.sap.rule.engine;

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
public abstract class AbstractRule implements BaseRule {
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

    public AbstractRule(String id, int priority, boolean exclusive, boolean repeat) {
        this.id = id;
        this.priority = priority;
        this.exclusive = exclusive;
        this.repeat = repeat;
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
    public boolean isRepeat() {
        return repeat;
    }

    public void setRepeat(boolean repeat) {
        this.repeat = repeat;
    }

    @Override
    public boolean isExclusive() {
        return exclusive;
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
