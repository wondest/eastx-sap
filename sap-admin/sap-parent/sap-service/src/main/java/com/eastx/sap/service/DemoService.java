package com.eastx.sap.service;

import com.eastx.sap.batch.demo.DemoJobConfiguration;
import com.eastx.sap.data.vo.JobStatusVO;
import com.eastx.sap.error.ErrorEnum;
import com.eastx.sap.error.ExceptionFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersInvalidException;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

/**
 * @ClassName DemoService
 * @Description: TODO
 * @Author Tender
 * @Time 2022/2/19 18:16
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
@Component
public class DemoService {
    @Autowired
    ExceptionFactory exceptionFactory;

    @Autowired
    JobLauncher jobLauncher;

    @Autowired
    JobExplorer jobExplorer;

    @Autowired
    DemoJobConfiguration demoJobConfiguration;

    @Qualifier("demoJob")
    @Autowired
    Job demoJob;

    public JobStatusVO runJob(String bizDate) {

        JobStatusVO jobStatus = new JobStatusVO();
        try {
            JobExecution jobExecution = jobLauncher.run(demoJob
                    ,demoJobConfiguration.createParameters(bizDate));

            jobStatus.setJobInstanceId(jobExecution.getJobId());
            jobStatus.setJobExecutionId(jobExecution.getId());
            jobStatus.setJobStatus(mapJobStatus(jobExecution.getStatus()));
        } catch (JobExecutionAlreadyRunningException e) {
            log.info("Spring-batch launch job already running", e);
            jobStatus.setJobStatus(JobStatusVO.ALREADY_RUNNING);
        } catch (JobInstanceAlreadyCompleteException e) {
            log.info("Spring-batch launch job already completed", e);
            jobStatus.setJobStatus(JobStatusVO.ALREADY_COMPLETED);
        } catch (Exception e) {
            log.error("Spring-batch launch job failed", e);
            exceptionFactory.newException(ErrorEnum.BATCH_ERROR, e);
        }

        return jobStatus;
    }

    /**
     *
     * @param jobExecutionId
     * @return
     */
    public JobStatusVO queryJob(Long jobExecutionId) {

        JobStatusVO jobStatus = new JobStatusVO();

        JobExecution jobExecution = jobExplorer.getJobExecution(jobExecutionId);

        jobStatus.setJobInstanceId(jobExecution.getJobId());
        jobStatus.setJobExecutionId(jobExecution.getId());
        jobStatus.setJobStatus(mapJobStatus(jobExecution.getStatus()));

        return jobStatus;

    }

    /**
     * R
     * @param status
     * @return
     */
    private String mapJobStatus(BatchStatus status) {

        if(BatchStatus.COMPLETED.equals(status)) {
            return JobStatusVO.COMPLETED;
        } else if(status.isRunning()) {
            return JobStatusVO.RUNNING;
        } else {
            return JobStatusVO.FAILED;
        }
    }
}
