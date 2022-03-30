package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.evaluator.AbstractCompositeEvaluator;
import com.eastx.sap.rule.core.evaluator.AbstractEvaluator;
import com.eastx.sap.rule.core.evaluator.AbstractSimpleEvaluator;
import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.engine.BeanRule;
import com.eastx.sap.rule.engine.Rule;
import com.eastx.sap.rule.model.ExpressionQueue;
import com.eastx.sap.rule.model.OperandEnum;
import org.springframework.util.Assert;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Optional;

/**
 * @ClassName JavaRuleBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 17:29
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class JavaRuleBuilder {
    /**
     * The stack for operator
     */
    private Deque<AbstractEvaluator> operatorStack = new ArrayDeque<>();

    /**
     * The stack for operand
     */
    private Deque<OperandEnum> operandStack = new ArrayDeque<OperandEnum>();

    /**
     * The queue for expression
     */
    private RuleBuilderHelper builder;

    public JavaRuleBuilder(RuleBuilderHelper builder) {
        Assert.notNull(builder, "builder should not be null");
        this.builder = builder;
    }

    /**
     *
     * @param evaluator
     * @return
     */
    private AbstractCompositeEvaluator wrap(Evaluator evaluator) {
        if(evaluator instanceof AbstractCompositeEvaluator.BodySelf) {
            return (AbstractCompositeEvaluator)evaluator;
        } else {
            return new AbstractCompositeEvaluator.BodySelf(evaluator);
        }
    }

    /**
     *
     * @param evaluator
     * @return
     */
    private AbstractCompositeEvaluator wrapAnd(Evaluator evaluator) {
        if(evaluator instanceof AbstractCompositeEvaluator.BodyAnd) {
            return (AbstractCompositeEvaluator)evaluator;
        } else {
            return new AbstractCompositeEvaluator.BodyAnd(evaluator);
        }
    }

    /**
     *
     * @param evaluator
     * @return
     */
    private AbstractCompositeEvaluator wrapOr(Evaluator evaluator) {
        if(evaluator instanceof AbstractCompositeEvaluator.BodyOr) {
            return (AbstractCompositeEvaluator)evaluator;
        } else {
            return new AbstractCompositeEvaluator.BodyOr(evaluator);
        }
    }

    /**
     *
     * @param evaluator
     * @return
     */
    private AbstractCompositeEvaluator wrapNot(Evaluator evaluator) {
        return new AbstractCompositeEvaluator.BodyNot(evaluator);
    }

    /**
     * (1) 操作数入值栈
     * (2.1) 操作符入符号栈，如果当前操作符与当前栈顶符号对比，优先级高>，入栈；优先级低<=，计算，入栈；
     * (2.2) 操作符入符号栈，如果当前是end操作符，计算，不入栈，end是最低优先级
     * (2.3) 只有一个操作数：x END x入栈，END遇到NAN，NAN不弹出，NAN优先级比END高
     * (2.4) END操作符，直接break退出
     *
     * (NOT) 还不支持 TODO
     */
    private AbstractEvaluator getExpression(ExpressionQueue queue) {
        //循环计算表达式的值
        while(!queue.isEmpty()) {
            //从队列头弹出一个元素
            Object element = queue.poll();

            if(element instanceof AbstractEvaluator) {
                //操作数入栈
                operatorStack.push((AbstractEvaluator)element);
            } else {
                //操作符处理
                OperandEnum operand = (OperandEnum)element;

                //when null then give the lowest priority operand
                OperandEnum topOperand = Optional.ofNullable(operandStack.peek()).orElse(OperandEnum.NAN);

                //(不高于栈中的运算符优先级）入栈
                if(operand.getPriority() <= topOperand.getPriority()) {
                    evalOperator(topOperand);

                    //非NAN操作符才出栈
                    if(!topOperand.equals(OperandEnum.NAN)) {
                        //运算符出栈
                        operandStack.pop();
                    }
                }

                //遇到END运算符直接break
                if(OperandEnum.END.equals(operand)) {
                    break;
                } else {
                    //新操作符入栈
                    operandStack.push(operand);
                }
            }
        }

        //清空操作符号栈
        while(!operandStack.isEmpty()) {
            evalOperator(operandStack.pop());
        }

        //弹出最终结果
        return operatorStack.pop();
    }

    /**
     *
     * @param operand
     */
    private void evalOperator(OperandEnum operand) {
        //计算 入栈
        if(OperandEnum.AND.equals(operand)) {
            AbstractCompositeEvaluator operator1 = wrapAnd(operatorStack.pop());
            AbstractCompositeEvaluator operator2 = wrapAnd(operatorStack.pop());

            //操作数计算
            operator1.extend(operator2);

            //操作数入栈
            operatorStack.push(operator1);
        } else if(OperandEnum.OR.equals(operand)) {
            AbstractCompositeEvaluator operator1 = wrapOr(operatorStack.pop());
            AbstractCompositeEvaluator operator2 = wrapOr(operatorStack.pop());

            //操作数计算
            operator1.extend(operator2);

            //操作数入栈
            operatorStack.push(operator1);
        } else if(OperandEnum.NOT.equals(operand)) {
            AbstractCompositeEvaluator operator1 = wrapNot(operatorStack.pop());

            //操作数入栈
            operatorStack.push(operator1);
        }
    }

    /**
     * Build a rule
     */
    public Rule build() {
        return new BeanRule(builder.getId(),
                builder.getPriority(),
                getExpression(builder.getExpressionQueue()),
                builder.getProcessor());
    }
}
