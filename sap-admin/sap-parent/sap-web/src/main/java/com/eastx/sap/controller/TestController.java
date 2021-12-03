package com.eastx.sap.controller;

import com.eastx.sap.annotation.LogTest;
import com.eastx.sap.error.ErrorEnum;
import com.eastx.sap.error.ExceptionFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * @ClassName TestController
 * @Description: TODO
 * @Author Tender
 * @Time 2021/12/2 23:24
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
@RestController
@EnableBatchProcessing
@RequestMapping("test")
public class TestController {
    @Autowired
    ExceptionFactory exceptionFactory;

    @GetMapping(value="/aop/return")
    @LogTest(value = "111")
    public int getReturn(){
        log.info("Test aop, return 1");
        return 1;
    }

    @GetMapping(value="/aop/throw")
    @LogTest(value = "222")
    public int getThrow(){
        log.info("Test aop, exception");
        throw exceptionFactory.newException(ErrorEnum.FAIL, "test exception");
    }
}
