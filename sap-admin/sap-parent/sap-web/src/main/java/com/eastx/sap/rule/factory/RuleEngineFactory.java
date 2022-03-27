package com.eastx.sap.rule.factory;

import com.eastx.sap.rule.core.parser.ExpressionParsers;
import com.eastx.sap.rule.engine.DefaultRuleEngine;
import com.eastx.sap.rule.engine.DefaultRuleExecutor;
import com.eastx.sap.rule.engine.PerfRuleEngine;
import com.eastx.sap.rule.engine.RuleEngine;

/**
 * @ClassName DefaultRuleEngineFactory
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/20 9:13
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class RuleEngineFactory {
    /**
     *
     * @param <F>
     * @return
     */
    public<F> RuleEngine<F> getDefaultEngine() {
        return new DefaultRuleEngine<>(new DefaultRuleExecutor<>(ExpressionParsers.java()));
    }

    /**
     *
     * @param maxRuns
     * @param <F>
     * @return
     */
    public<F> RuleEngine<F> getPerfEngine(int maxRuns) {
        return new PerfRuleEngine<>(new DefaultRuleExecutor<>(ExpressionParsers.java()), maxRuns);
    }
}
