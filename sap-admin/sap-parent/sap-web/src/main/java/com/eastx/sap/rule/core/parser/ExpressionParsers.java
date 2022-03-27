package com.eastx.sap.rule.core.parser;

/**
 * @ClassName ExpressionParsers
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/26 21:10
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class ExpressionParsers {
    /**
     *
     * @return
     */
    public static ExpressionParser java() {
        return new JavaParser();
    }

    /**
     *
     * @return
     */
    public static ExpressionParser mvel() {
        return new MvelParser();
    }

    /**
     *
     * @return
     */
    public static ExpressionParser spel() {
        return new SpelParser();
    }
}
