package com.eastx.sap.rule.engine;

import com.eastx.sap.rule.core.processor.Processor;

import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * @ClassName definition
 * @Description:
 *  参考Drools,定义规则
 *
 *     When condition -- evaluator
 *     Then action -- processor
 *
 *  evaluator 设计：
 *      根据输入的事实和参数，进行真值判断
 *
 * @Author Tender
 * @Time 2022/3/13 8:35
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class LangRule extends AbstractRule implements Rule {
    /**
     * 规则求值器-Condition
     */
    private final String evaluator;

    /**
     * 规则处理器-Action
     */
    private final Processor processor;

    public LangRule(String id, int priority, String evaluator, Processor processor) {
        super(id, priority, true, false);
        this.evaluator = evaluator;
        this.processor = processor;
    }

    @Override
    public Supplier getCondition() {
        return ()->evaluator;
    }

    @Override
    public Consumer getAction() {
        return (fact)->processor.execute(fact);
    }
}
