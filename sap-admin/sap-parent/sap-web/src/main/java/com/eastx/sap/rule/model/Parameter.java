package com.eastx.sap.rule.model;

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
public interface Parameter<T> extends Serializable {
    /**
     *
     * @param fact fact的值
     * @return
     */
    boolean check(T fact);

    /**
     * @param fact fact的名称
     * @return
     */
    String getMvel(String fact);

    /**
     * @param fact fact的名称
     * @return
     */
    String getSpel(String fact);

    /**
     *
     * @return
     */
    Map<String, Object> getEntry();
}
