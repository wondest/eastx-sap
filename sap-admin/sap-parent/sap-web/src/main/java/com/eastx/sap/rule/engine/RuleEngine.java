package com.eastx.sap.rule.engine;

/**
 * @ClassName RuleEngine
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/20 8:29
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface RuleEngine {
    /**
     * 执行规则集
     * @param context
     */
    void execute(Context context);

    /**
     * 装配规则集
     */
    void assemble(RuleSet rules);
}
