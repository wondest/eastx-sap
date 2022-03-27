package com.eastx.sap.rule.engine;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

/**
 * @param <R>
 */
public class DefaultRuleSet<R extends BaseRule> implements RuleSet {
    /**
     * 规则集容器
     */
    private Map<String, Rule> rules = new HashMap<>();

    @Override
    public boolean add(Rule rule) {
        if (rules.containsKey(rule.getId())) {
            return false;
        } else {
            //多并发情况下还是有问题
            rules.put(rule.getId(), rule);
            return true;
        }
    }

    @Override
    public boolean remove(Rule rule) {
        return (null != rules.remove(rule.getId()));
    }

    @Override
    public void forEach(Consumer<Rule> action) {
        rules.entrySet().stream().map(e -> e.getValue())
                .sorted(Comparator.comparing(Rule::getPriority).reversed())
                .forEach(action);
    }

    @Override
    public boolean isEmpty() {
        return rules.isEmpty();
    }

    @Override
    public int size() {
        return rules.size();
    }

    @Override
    public Collection<Rule> getRules() {
        return rules.values();
    }
}
