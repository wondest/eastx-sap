package com.eastx.sap.rule.core;

import com.eastx.sap.rule.evaluator.Evaluator;
import com.eastx.sap.rule.processor.Processor;

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
public class SimpleRule<T> implements Rule<T> {
    /**
     * 求值器
     */
    Evaluator<T> evaluator;

    /**
     * 处理器
     */
    Processor processor;

    public SimpleRule(Evaluator evaluator, Processor processor) {
        this.evaluator = evaluator;
        this.processor = processor;
    }

    /**
     *
     */
    @Override
    public void fire(T fact) {
        if(evaluator.eval(fact)) {
            processor.execute();
        }
    }
}
