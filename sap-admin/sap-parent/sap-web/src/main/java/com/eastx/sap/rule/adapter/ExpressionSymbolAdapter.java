package com.eastx.sap.rule.adapter;

/**
 *
 */
public interface ExpressionSymbolAdapter {
    /**
     * Boolean and
     * @return
     */
    String and();

    /**
     * Boolean or
     * @return
     */
    String or();

    /**
     * Boolean not
     * @return
     */
    String not();

    /**
     * variable
     * @return
     */
    String variable(String name);

    /**
     * Arithmetic 不等于
     */
    String notEqual();

    /**
     * Arithmetic 等于
     */
    String equalTo();

    /**
     * Arithmetic 大于
     */
    String greaterThan();

    /**
     * Arithmetic 大于等于
     */
    String greaterOrEqual();

    /**
     * Arithmetic 小于
     */
    String lessThan();

    /**
     * Arithmetic 小于等于
     */
    String lessOrEqual();

    /**
     * 空格
     */
    String space();
}
