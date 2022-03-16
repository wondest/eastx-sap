package com.eastx.sap.rule.core;

import com.eastx.sap.rule.data.BaseFact;
import com.eastx.sap.rule.evaluator.Evaluator;
import com.eastx.sap.rule.processor.Processor;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

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
public class SimpleRule<F extends BaseFact> implements Rule<F>, InitializingBean {
    /**
     * 规则求值器
     */
    Evaluator<F> evaluator;

    /**
     * 规则处理器
     */
    Processor processor;

    public SimpleRule(Evaluator<F> evaluator, Processor processor) {
        this.evaluator = evaluator;
        this.processor = processor;
    }

    /**
     * 发射规则
     */
    @Override
    public void fire(F fact) {
        if(evaluator.eval(fact)) {
            processor.execute(fact);
            fact.setAccept(true);
        } else {
            fact.setAccept(false);
        }
    }

    @Override
    public void afterPropertiesSet() {
        Assert.notNull(evaluator, "evaluator should not be null");
        Assert.notNull(processor, "processor should not be null");
    }
}
