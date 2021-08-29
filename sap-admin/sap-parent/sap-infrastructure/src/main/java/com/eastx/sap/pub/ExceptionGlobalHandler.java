package com.eastx.sap.pub;

import com.eastx.sap.error.ExceptionFactory;
import com.eastx.sap.error.ExtendRuntimeException;
import com.eastx.sap.error.ErrorEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * @ClassName Advice
 * @Description: TODO
 * @Author Tender
 * @Time 2021/7/14 0:19
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/

@ControllerAdvice
@ResponseBody
@Slf4j
public class ExceptionGlobalHandler {
    @Autowired
    ExceptionFactory exceptionFactory;

    @ExceptionHandler
    public ResponseData runtimeException(ExtendRuntimeException e) {
        return ResponseData.error(e);
    }

    @ExceptionHandler
    public ResponseData unknownException(Exception e) {
        return ResponseData.error(exceptionFactory.newException(ErrorEnum.FAIL, new Object[] {e.getMessage()}));
    }
}
