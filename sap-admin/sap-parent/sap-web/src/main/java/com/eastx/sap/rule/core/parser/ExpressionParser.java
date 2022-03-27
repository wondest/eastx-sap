package com.eastx.sap.rule.core.parser;

/**
 * @ClassName expression
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 10:52
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface ExpressionParser {
    /**
     *
     * @param expression
     * @return
     */
    ExpressionAdapter parseExpression(Object expression);
}
