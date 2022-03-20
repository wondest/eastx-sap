package com.eastx.sap.rule.engine;

/**
 * @ClassName Context
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/17 0:01
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface Context<F> {
    /**
     *
     * @return
     */
    F getFact();

    /**
     *
     * @param rule
     */
    void hitRule(Rule<F> rule);

    /**
     *
     * @param rule
     */
    default void passRule(Rule<F> rule) {
        throw new UnsupportedOperationException("pass the rule");
    }

    /**
     *
     * @param rule
     */
    default void rejectRule(Rule<F> rule) {
        throw new UnsupportedOperationException("reject the rule");
    }

    /**
     *
     * @param finish
     */
    void setFinish(boolean finish);

    /**
     *
     */
    boolean isFinished();

    /**
     *
     * @return
     */
    boolean isAccept();
}
