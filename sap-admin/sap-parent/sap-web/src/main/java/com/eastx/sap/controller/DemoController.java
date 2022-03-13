package com.eastx.sap.controller;

import cn.hutool.core.lang.Assert;
import com.eastx.sap.annotation.LogTest;
import com.eastx.sap.data.vo.JobStatusVO;
import com.eastx.sap.error.ErrorEnum;
import com.eastx.sap.error.ExceptionFactory;
import com.eastx.sap.service.DemoService;
import lombok.Data;
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
@RequestMapping("demo")
public class DemoController {
    @Autowired
    ExceptionFactory exceptionFactory;

    @Autowired
    DemoService demoService;

    @GetMapping(value="/job/run")
    public JobStatusVO runJob(@RequestParam(required = true)String bizDate){

        Assert.notBlank(bizDate, ()->exceptionFactory.newException(ErrorEnum.PARAM_NULL, "bizDate"));

        return demoService.runJob(bizDate);
    }

    @GetMapping(value="/job/query")
    public JobStatusVO queryJob(@RequestParam(required = true)Long jobExecutionId){
        log.info("run job success");
        Assert.notNull(jobExecutionId, ()->exceptionFactory.newException(ErrorEnum.PARAM_NULL, "jobExecutionId"));

        return demoService.queryJob(jobExecutionId);
    }

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
