package com.eastx.sap.rule.engine;

/**
 * 所有规则共用的 规则元数据
 */
public interface BaseRule {
    /**
     * 股则编号
     *
     * @return
     */
    String getId();

    /**
     * 规则优先级
     *
     * @return
     */
    int getPriority();

    /**
     * 规则是否排他
     *
     * @return
     */
    boolean isExclusive();

    /**
     * 规则是否可重复
     *
     * @return
     */
    boolean isRepeat();
}
