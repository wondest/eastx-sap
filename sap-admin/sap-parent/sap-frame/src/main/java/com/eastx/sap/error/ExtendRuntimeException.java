package com.eastx.sap.error;

/**
 * @ClassName ExtendRuntimeException
 * @Description: 传入
 * @Author Tender
 * @Time 2021/7/13 22:16
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class ExtendRuntimeException extends RuntimeException {
    /**
     * 错误码
     */
    private String code;

    ExtendRuntimeException(String code, String message, Throwable cause) {
        super(message, cause);
        this.code = code;
    }

    ExtendRuntimeException(String code, String message) {
        super(message);
        this.code = code;
    }

    public String getCode() {
        return code;
    }
}
