package com.eastx.sap.rule.core.parameter;

import com.eastx.sap.rule.adapter.ExpressionSymbolAdapter;

import java.io.Serializable;
import java.util.Map;

/**
 * @ClassName ParameterPO
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 8:28
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface Parameter<V> extends Serializable {
    /**
     *
     * @param fact the value of a fact
     * @return
     */
    boolean check(V fact);

    /**
     *
     * @param fact the name of a fact
     * @return
     */
    String getExpression(String fact, ExpressionSymbolAdapter adapter);
}
