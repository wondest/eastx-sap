package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.processor.Processor;
import com.eastx.sap.rule.model.ArrayExpressionDeque;
import com.eastx.sap.rule.model.ExpressionQueue;

/**
 * @ClassName RuleBuilderHelper
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 9:02
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public abstract class RuleBuilderHelper<B extends RuleBuilderHelper> {
    /**
     * The rule id
     */
    protected String id;

    /**
     * The rule priority
     */
    protected int priority;

    /**
     * The stack for bool expression,the object is [ operand->OperandEnum, operator->AbstractChainedEvaluator ]
     */
    private ExpressionQueue expressionQueue = new ArrayExpressionDeque();

    /**
     * Set the rule action processor
     */
    private Processor processor;

    /**
     * Concat the expression elements
     *
     * @param condition Operand:OperandEnum or Operator:AbstractCompositeEvaluator
     * @return
     */
    B concat(Object condition) {
        expressionQueue.offer(condition);
        return (B)this;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public int getPriority() {
        return priority;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }

    public Processor getProcessor() {
        return processor;
    }

    public void setProcessor(Processor processor) {
        this.processor = processor;
    }

    public ExpressionQueue getExpressionQueue() {
        return expressionQueue;
    }
}
