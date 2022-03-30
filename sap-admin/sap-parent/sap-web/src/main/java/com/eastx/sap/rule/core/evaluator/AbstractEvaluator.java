package com.eastx.sap.rule.core.evaluator;

/**
 * @ClassName AbstractEvaluator
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/28 23:03
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public abstract class AbstractEvaluator implements Evaluator {
    /**
     * 反转标志 - 对应Not运算
     */
    private boolean reversed;

    /**
     *
     * @return
     */
    @Override
    public Evaluator not() {
        reversed = true;
        return this;
    }

    /**
     *
     * @return
     */
    @Override
    public boolean isReversed() {
        return reversed;
    }
}
