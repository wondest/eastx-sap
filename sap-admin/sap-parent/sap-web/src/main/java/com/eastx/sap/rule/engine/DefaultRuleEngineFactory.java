package com.eastx.sap.rule.engine;

/**
 * @ClassName DefaultRuleEngineFactory
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/20 9:13
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class DefaultRuleEngineFactory {

    public<F> RuleEngine<F> getDefaultEngine() {
        return new DefaultRuleEngine<F>(new DefaultRuleExecutor<F>());
    }

    public<F> RuleEngine<F> getPerfEngine(int maxRuns) {
        return new PerfRuleEngine<F>(new DefaultRuleExecutor<F>(), maxRuns);
    }

}
