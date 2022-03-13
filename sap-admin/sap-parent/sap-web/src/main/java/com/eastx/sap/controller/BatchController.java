package com.eastx.sap.controller;

import com.eastx.sap.batch.mybatisJob.MybatisJobFactory;
import com.eastx.sap.batch.tdxJob.OrderJobFactory;
import com.eastx.sap.batch.batchTdx.TdxJobFactory;
import com.eastx.sap.data.mapper.StockMapper;
import com.eastx.sap.data.model.Stock;
import com.eastx.sap.error.ErrorEnum;
import com.eastx.sap.error.ExceptionFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.text.SimpleDateFormat;

/**
 * @ClassName StockController
 * @Description: 提供批量操作相关的接口
 * @Author Tender
 * @Time 2021/7/11 11:18
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
@RestController
@EnableBatchProcessing
@RequestMapping("batch")
public class BatchController {
    @Autowired
    ExceptionFactory exceptionFactory;

    @Autowired
    JobLauncher jobLauncher;

    @Autowired
    TdxJobFactory jobFactory;

    @Autowired
    StockMapper stockMapper;

    @Autowired
    MybatisJobFactory mybatisJobFactory;

    @Autowired
    Job mybatisJob;

    /**
     * 群发消息内容
     * @return
     */
    @RequestMapping(value="/job", method= RequestMethod.GET)
    public void execute(@RequestParam(required=true) String code, @RequestParam(required=false) String bizDate, @RequestParam(required=false) String batchNo) {
        try {
            SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyyMMdd");
            jobLauncher.run(jobFactory.createJob(code), jobFactory.createParameters(code, dateFormatter.parse(bizDate)));
            
        } catch(Exception e) {
            log.error("jobLauncher.run failed", e);
            throw exceptionFactory.newException(ErrorEnum.FAIL, e);
        }
    }

    /**
     * 群发消息内容
     * @return
     */
    @RequestMapping(value="/stock", method= RequestMethod.GET)
    public void getStock(@RequestParam(required=false) Long id) {
        try {
            JobExecution jobExecution = jobLauncher.run(mybatisJob, mybatisJobFactory.createParameters(1, id + "aaa.dat"));


            if(jobExecution.getStatus() == BatchStatus.COMPLETED) {
                log.info("!!! JOB FINISHED! Time to verify the results");
                log.info(jobExecution.getJobParameters().getString("id"));
            } else {
                log.info("!!! JOB Failed! Time to verify the results");
            }
            
        } catch(Exception e) {
            log.error("jobLauncher.run failed", e);
            throw exceptionFactory.newException(ErrorEnum.FAIL, e);
        }
    }
}
