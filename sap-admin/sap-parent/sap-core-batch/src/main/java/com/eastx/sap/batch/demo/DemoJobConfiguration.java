package com.eastx.sap.batch.demo;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @ClassName DemoJobConfig
 * @Description: TODO
 * @Author Tender
 * @Time 2022/2/19 17:56
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Configuration
@EnableBatchProcessing
public class DemoJobConfiguration {
    @Autowired
    public JobBuilderFactory jobBuilderFactory;

    @Autowired
    public StepBuilderFactory stepBuilderFactory;

    @Autowired
    DemoSleepTasklet demoSleepTasklet;

    @Bean(name="demoJob")
    public Job createJob(Step demoStep1) {
        return jobBuilderFactory.get("demoJob")
                .incrementer(new RunIdIncrementer())
                .flow(demoStep1)
                .end()
                .build();
    }

    @Bean
    public Step demoStep1(DemoSleepTasklet demoSleepTasklet) {
        return stepBuilderFactory.get("step1")
                .tasklet(demoSleepTasklet)
                .build();
    }

    /**
     * Create Parameters
     *
     * @return
     */
    public JobParameters createParameters(String bizDate) {
        JobParameters parameters = new JobParametersBuilder()
                .addString("bizDate", bizDate)
                .toJobParameters();

        return parameters;
    }
}
