package com.eastx.sap.rule.data;

import java.io.Serializable;

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
    boolean check(T value);
}
