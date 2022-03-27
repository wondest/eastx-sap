package com.eastx.sap.rule.engine;

import java.util.Collection;
import java.util.function.Consumer;

/**
 * 规则集
 *
 * @param <R>
 */
public interface RuleSet {
    /**
     * 增加规则
     * @param rule
     */
    boolean add(Rule rule);

    /**
     * 移除规则
     * @param rule
     */
    boolean remove(Rule rule);

    /**
     * 遍历规则集
     * @param action
     */
    void forEach(Consumer<Rule> action);

    /**
     * 规则集是否为空
     * @return
     */
    boolean isEmpty();

    /**
     * 包含规则数
     * @return
     */
    int size();

    /**
     * 提取所有规则
     * @return
     */
    Collection<Rule> getRules();
}
