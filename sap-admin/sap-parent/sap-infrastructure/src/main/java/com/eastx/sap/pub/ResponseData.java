package com.eastx.sap.pub;

import com.eastx.sap.error.ExtendRuntimeException;
import lombok.Data;

import java.util.Collection;

/**
 * @ClassName ResponseData
 * @Description: TODO
 * @Author Tender
 * @Time 2021/7/14 0:24
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Data
public class ResponseData<T> {
    /**
     * 返回状态
     */
    private Boolean success;

    /**
     * 错误码
     */
    private String errorCode;

    /**
     * 错误信息
     */
    private String errorMessage;

    /**
     * 返回数据
     */
    private T data;

    /**
     * 分页时候使用
     */
    private Integer total;

    /**
     * Return error
     * @param code
     * @param message
     * @return
     */
    public static ResponseData error(String code, String message) {
        ResponseData result = new ResponseData();
        result.setSuccess(false);
        result.setErrorCode(code);
        result.setErrorMessage(message);
        return result;
    }

    /**
     * Return error using exception
     * @param e
     * @return
     */
    public static ResponseData error(ExtendRuntimeException e) {
        return ResponseData.error(e.getCode(), e.getMessage());
    }

    /**
     * Return success
     * @return
     */
    public static ResponseData success() {
        ResponseData result = new ResponseData();
        result.setSuccess(true);
        return result;
    }

    /**
     * Return success with data
     * @param data
     * @return
     */
    public static <E> ResponseData<E> success(E data) {
        ResponseData result = new ResponseData();
        result.setSuccess(true);
        result.setData(data);
        return result;
    }

    /**
     * Return success with data and total
     * @param data
     * @param total
     * @return
     */
    public static <E> ResponseData<E> success(Collection<E> data, int total) {
        ResponseData result = new ResponseData();
        result.setSuccess(true);
        result.setData(data);
        result.setTotal(total);
        return result;
    }
}
