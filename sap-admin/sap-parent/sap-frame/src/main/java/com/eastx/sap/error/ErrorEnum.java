package com.eastx.sap.error;

/**
 * @ClassName StateCodeEnum
 * @Description: TODO
 * @Author Tender
 * @Time 2021/7/14 0:40
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public enum ErrorEnum {
    PARAM_NULL("E01"),
    BATCH_ERROR("E02"),
    ASYNC_RUNNING("501"),
    SUCCESS("200"),
    FAIL("500");

    private String code;

    ErrorEnum(String code) {
        this.code = code;
    }

    public String getCode() {
        return code;
    }

    public String getI18n() {
        return new StringBuffer("error.").append(code).toString();
    }
}
