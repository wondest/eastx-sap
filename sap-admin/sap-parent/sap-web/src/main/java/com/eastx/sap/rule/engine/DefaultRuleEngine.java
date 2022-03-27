package com.eastx.sap.rule.engine;

import lombok.extern.slf4j.Slf4j;

import java.util.function.Consumer;

/**
 * @param <F>
 */
@Slf4j
public class DefaultRuleEngine<F> implements RuleEngine<F> {
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
     * @param executor
     */
    public DefaultRuleEngine(RuleExecutor<F> executor) {
        this.executor = executor;
    }

    @Override
    public void execute(Context<F> context) {
        if (rules.isEmpty()) {
            return;
        }

        //遍历规则
        forEachRule(rule -> {
            log.info(String.format("execute - rule:%s", rule.getId()));
            if (!context.isFinished()) {
                boolean executed = executor.execute(context, rule);
                if (executed) {
                    //命中
                    log.info(String.format("hit - rule:%s", rule.getId()));
                    context.hitRule(rule);
                    if (rule.isExclusive()) {
                        context.setFinish(true);
                    }
                } else {
                    //拒绝
                    log.info(String.format("reject - rule:%s", rule.getId()));
                    context.rejectRule(rule);
                }
            } else {
                //跳过
                log.info(String.format("pass - rule:%s", rule.getId()));
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
