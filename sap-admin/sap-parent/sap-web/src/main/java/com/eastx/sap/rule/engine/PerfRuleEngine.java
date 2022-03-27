package com.eastx.sap.rule.engine;

import lombok.extern.slf4j.Slf4j;

import java.util.function.Consumer;
import java.util.stream.IntStream;

/**
 * 非功能
 *
 * @param <F>
 */
@Slf4j
public class PerfRuleEngine<F> implements RuleEngine<F> {
    /**
     * 规则执行器
     */
    private final RuleExecutor<F> executor;

    /**
     * 规则集
     */
    private RuleSet rules;

    /**
     *
     */
    private final int runLimiter;

    /**
     *
     * @param executor
     */
    public PerfRuleEngine(RuleExecutor<F> executor) {
        this(executor, 10000);
    }

    /**
     *
     * @param executor
     */
    public PerfRuleEngine(RuleExecutor<F> executor, int runLimiter) {
        this.executor = executor;
        this.runLimiter = runLimiter;
    }

    @Override
    public void execute(Context<F> context) {
        IntStream.range(0, runLimiter).forEach(i->doExecute(context));
    }

    /**
     *
     * @param context
     */
    private void doExecute(Context<F> context) {
        if (rules.isEmpty()) {
            return;
        }

        //遍历规则
        forEachRule(rule -> {
            if (true) {
                boolean executed = executor.execute(context, rule);
                if (executed) {
                    //命中
                    context.hitRule(rule);
                    if (rule.isExclusive()) {
                        context.setFinish(true);
                    }
                } else {
                    //拒绝
                    context.rejectRule(rule);
                }
            } else {
                //跳过
                context.passRule(rule);
            }
        });

        //结束
        context.setFinish(true);
    }

    @Override
    public void assemble(RuleSet ruleSet) {
        this.rules = ruleSet;
    }

    /**
     * @param action
     */
    private void forEachRule(Consumer<Rule> action) {
        rules.forEach(action);
    }
}
