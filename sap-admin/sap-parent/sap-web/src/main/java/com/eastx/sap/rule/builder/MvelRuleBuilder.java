package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.adapter.MvelExpressionSymbolAdapter;
import com.eastx.sap.rule.engine.LangRule;
import com.eastx.sap.rule.engine.Rule;
import org.springframework.util.Assert;

/**
 * @ClassName MvelRuleBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 17:29
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class MvelRuleBuilder extends LangRuleHelper {
    /**
     * The queue for expression
     */
    private RuleBuilderHelper builder;

    public MvelRuleBuilder(RuleBuilderHelper builder) {
        super(new MvelExpressionSymbolAdapter());
        Assert.notNull(builder, "builder should not be null");
        this.builder = builder;
    }

    /**
     * Build a rule
     */
    public Rule build() {
        return new LangRule(builder.getId(),
                builder.getPriority(),
                getExpression(builder.getExpressionQueue()),
                builder.getProcessor());
    }
}
