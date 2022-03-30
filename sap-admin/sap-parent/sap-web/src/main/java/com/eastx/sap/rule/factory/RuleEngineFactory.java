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
     * @return
     */
    public RuleEngine java() {
        return new DefaultRuleEngine(new DefaultRuleExecutor(ExpressionParsers.java()));
    }

    /**
     *
     * @return
     */
    public RuleEngine spel() {
        return new DefaultRuleEngine(new DefaultRuleExecutor(ExpressionParsers.spel()));
    }

    /**
     *
     * @return
     */
    public RuleEngine mvel() {
        return new DefaultRuleEngine(new DefaultRuleExecutor(ExpressionParsers.mvel()));
    }

    /**
     *
     * @param maxRuns
     * @return
     */
    public RuleEngine javaPerf(int maxRuns) {
        return new PerfRuleEngine(new DefaultRuleExecutor(ExpressionParsers.java()), maxRuns);
    }
}
