package com.eastx.sap.rule.engine;

import java.util.LinkedList;
import java.util.List;

/**
 * @ClassName Context
 * @Description:
 *
 *  上下文
 *
 * @Author Tender
 * @Time 2022/3/16 23:27
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class DefaultContext<F> implements Context<F> {
    /**
     * 上下文事实
     */
    private final F fact;

    /**
     * 是否结束
     */
    private boolean finish;

    /**
     * 接受的规则
     */
    private List<Rule<F>> acceptRules;

    /**
     * 拒绝的规则
     */
    private List<Rule<F>> rejectRules;

    /**
     * 跳过的规则
     */
    private int passCounter;

    public DefaultContext(F fact) {
        this.fact = fact;
        this.finish = false;
        this.acceptRules = new LinkedList<>();
        this.rejectRules = new LinkedList<>();
    }

    @Override
    public F getFact() {
        return this.fact;
    }

    @Override
    public void hitRule(Rule rule) {
        this.acceptRules.add(rule);
    }

    @Override
    public void rejectRule(Rule rule) {
        this.rejectRules.add(rule);
    }

    @Override
    public void passRule(Rule rule) {
        this.passCounter++;
    }

    @Override
    public void setFinish(boolean finish) {
        this.finish = finish;
    }

    @Override
    public boolean isFinished() {
        return finish;
    }

    @Override
    public boolean isAccept() {
        return (acceptRules.size() > 0);
    }

    @Override
    public String toString() {
        return "DefaultContext{" +
                "fact=" + fact +
                ", finish=" + finish +
                ", acceptRules=" + acceptRules +
                ", rejectRules=" + rejectRules +
                ", passCounter=" + passCounter +
                '}';
    }
}
