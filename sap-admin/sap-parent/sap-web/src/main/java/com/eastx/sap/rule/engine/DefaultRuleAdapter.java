package com.eastx.sap.rule.engine;

/**
 * @ClassName DefaultRuleAdapter
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/20 18:24
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface DefaultRuleAdapter extends BaseRule {
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
