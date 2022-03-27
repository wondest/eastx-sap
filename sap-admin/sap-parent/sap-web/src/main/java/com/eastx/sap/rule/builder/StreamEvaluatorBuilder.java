package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.model.OperandEnum;
import com.eastx.sap.rule.core.evaluator.AbstractCompositeEvaluator;
import com.eastx.sap.rule.core.evaluator.Evaluator;
import com.eastx.sap.rule.core.evaluator.ReverseEvaluatorWrapper;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Optional;

/**
 * @ClassName StreamEvaluatorBuilder
 * @Description:
 * 链式构造
 *   start(aa)
 *   start(aa).or(cc).and(dd)
 *   start(aa).and(cc).or(dd) => aa && cc || dd => (aa && cc) || dd
 *   start(aa).and(cc).and(dd) => aa && cc && dd
 *   start(aa).or(cc).or(dd) => aa || cc || dd
 * @Author Tender
 * @Time 2022/3/14 8:13
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class StreamEvaluatorBuilder {
    /**
     * The stack for bool expression,the object is [ operand->OperandEnum, operator->AbstractChainedEvaluator ]
     */
    private Deque<Object> expressionDeque = new ArrayDeque<Object>();

    /**
     * The stack for operator
     */
    private Deque<AbstractCompositeEvaluator> operatorStack = new ArrayDeque<AbstractCompositeEvaluator>();

    /**
     * The stack for operand
     */
    private Deque<OperandEnum> operandStack = new ArrayDeque<OperandEnum>();

    /**
     *
     * @param evaluator [ 普通的evaluator 或者 链式的evaluator ]
     * @return
     */
    public StreamEvaluatorBuilder start(Evaluator evaluator) {
        expressionDeque.offer(wrap(evaluator));
        return this;
    }

    /**
     *
     * @param evaluator [ 普通的evaluator 或者 链式的evaluator ]
     * @return
     */
    public StreamEvaluatorBuilder and(Evaluator evaluator) {
        expressionDeque.offer(OperandEnum.AND);
        expressionDeque.offer(wrap(evaluator));
        return this;
    }

    /**
     *
     * @param evaluator
     * @return
     */
    public StreamEvaluatorBuilder or(Evaluator evaluator) {
        expressionDeque.offer(OperandEnum.OR);
        expressionDeque.offer(wrap(evaluator));
        return this;
    }

    /**
     *
     * @param evaluator
     * @return
     */
    public StreamEvaluatorBuilder andNot(Evaluator evaluator) {
        expressionDeque.offer(OperandEnum.AND);
        expressionDeque.offer(wrap(new ReverseEvaluatorWrapper<>(evaluator)));
        return this;
    }

    /**
     *
     * @param evaluator
     * @return
     */
    public StreamEvaluatorBuilder orNot(Evaluator evaluator) {
        expressionDeque.offer(OperandEnum.OR);
        expressionDeque.offer(wrap(new ReverseEvaluatorWrapper<>(evaluator)));
        return this;
    }

    /**
     *
     * @return
     */
    private StreamEvaluatorBuilder end() {
        expressionDeque.offer(OperandEnum.END);
        return this;
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
     * @return
     */
    public Evaluator build() {
        end();
        return evalJavaExpression();
    }

    /**
     * (1) 操作数入值栈
     * (2.1) 操作符入符号栈，如果当前操作符与当前栈顶符号对比，优先级高>，入栈；优先级低<=，计算，入栈；
     * (2.2) 操作符入符号栈，如果当前是end操作符，计算，不入栈，end是最低优先级
     * (2.3) 只有一个操作数：1 END 1入栈，END遇到NAN，NAN不弹出，NAN优先级比END高
     * (2.4) END操作符，直接break退出
     */
    private AbstractCompositeEvaluator evalJavaExpression() {
        //循环计算表达式的值
        while(!expressionDeque.isEmpty()) {
            //从队列头弹出一个元素
            Object element = expressionDeque.poll();

            if(element instanceof AbstractCompositeEvaluator) {
                //操作数入栈
                operatorStack.push((AbstractCompositeEvaluator)element);
            } else {
                //操作符处理
                OperandEnum operand = (OperandEnum)element;

                //when null then give the lowest priority operand
                OperandEnum topOperand = Optional.ofNullable(operandStack.peek()).orElse(OperandEnum.NAN);

                //(不高于栈中的运算符优先级）入栈
                if(operand.getPriority() <= topOperand.getPriority()) {
                    //计算 入栈
                    if(OperandEnum.AND.equals(topOperand)) {
                        AbstractCompositeEvaluator operator1 = wrapAnd(operatorStack.pop());
                        AbstractCompositeEvaluator operator2 = wrapAnd(operatorStack.pop());

                        //操作数计算
                        operator1.extend(operator2);

                        //操作数入栈
                        operatorStack.push(operator1);
                    } else if(OperandEnum.OR.equals(topOperand)) {
                        AbstractCompositeEvaluator operator1 = wrapOr(operatorStack.pop());
                        AbstractCompositeEvaluator operator2 = wrapOr(operatorStack.pop());

                        //操作数计算
                        operator1.extend(operator2);

                        //操作数入栈
                        operatorStack.push(operator1);
                    }

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

        //弹出最终结果
        return operatorStack.pop();
    }
}
