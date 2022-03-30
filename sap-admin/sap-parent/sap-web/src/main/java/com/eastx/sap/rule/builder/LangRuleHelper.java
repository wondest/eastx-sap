package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.adapter.ExpressionSymbolAdapter;
import com.eastx.sap.rule.core.evaluator.AbstractEvaluator;
import com.eastx.sap.rule.model.ExpressionQueue;
import com.eastx.sap.rule.model.OperandEnum;
import org.springframework.util.Assert;

/**
 * @ClassName LangRuleHelper
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 17:29
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public abstract class LangRuleHelper {
    /**
     * The stack for operand
     */
    private final ExpressionSymbolAdapter adapter;

    public LangRuleHelper(ExpressionSymbolAdapter adapter) {
        Assert.notNull(adapter, "builder should not be null");
        this.adapter = adapter;
    }

    /**
     * (1) 操作数入值栈
     * (2.1) 操作符入符号栈，如果当前操作符与当前栈顶符号对比，优先级高>，入栈；优先级低<=，计算，入栈；
     * (2.2) 操作符入符号栈，如果当前是end操作符，计算，不入栈，end是最低优先级
     * (2.3) 只有一个操作数：1 END 1入栈，END遇到NAN，NAN不弹出，NAN优先级比END高
     * (2.4) END操作符，直接break退出
     */
    protected String getExpression(ExpressionQueue queue) {

        StringBuilder expression = new StringBuilder();

        //循环计算表达式的值
        while(!queue.isEmpty()) {
            //从队列头弹出一个元素
            Object element = queue.poll();

            if(element instanceof AbstractEvaluator) {
                AbstractEvaluator evaluator = (AbstractEvaluator) element;
                expression.append(evaluator.getExpression(adapter));
            } else {
                //操作符处理
                OperandEnum operand = (OperandEnum)element;

                if(OperandEnum.AND.equals(operand)) {
                    expression.append(adapter.space()).append(adapter.and()).append(adapter.space());
                } else if(OperandEnum.OR.equals(operand)) {
                    expression.append(adapter.space()).append(adapter.or()).append(adapter.space());
                } else if(OperandEnum.NOT.equals(operand)) {
                    expression.append(adapter.not());
                } else if(OperandEnum.END.equals(operand)) {
                    break;
                }
            }
        }

        return expression.toString();
    }
}
