package com.eastx.sap.rule.engine;

import com.eastx.sap.rule.core.parser.ExpressionParser;
import com.eastx.sap.rule.core.parser.ExpressionParsers;
import org.springframework.util.Assert;

import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * 默认的规则执行器
 *
 * @param <F>
 */
public class DefaultRuleExecutor<F> extends AbstractRuleExecutor<F> {
    /**
     *
     */
    private final ExpressionParser parser;

    public DefaultRuleExecutor(ExpressionParser parser) {
        Assert.notNull(parser, "The parser should not be null");
        this.parser = parser;
    }

    @Override
    protected boolean evalCondition(Context<F> context, Supplier condition) {
        return parser.parseExpression(condition.get()).getValue(context.getFact());
    }

    @Override
    protected void processAction(Context<F> context, Consumer action) {
        action.accept(context.getFact());
    }
}
