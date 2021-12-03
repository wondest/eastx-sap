package com.eastx.sap.batch.tdxJob;

import com.eastx.sap.data.entity.JobInstance;
import com.eastx.sap.data.entity.Stock;
import org.springframework.batch.core.*;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.item.database.JpaCursorItemReader;
import org.springframework.batch.item.database.JpaItemWriter;
import org.springframework.batch.item.database.builder.JpaCursorItemReaderBuilder;
import org.springframework.batch.item.database.builder.JpaItemWriterBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.Nullable;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;

/**
 * @ClassName OrderJobFactory
 * @Description: 实例化作业，读取作业基本信息，根据输入的日期等环境参数，进行实例化操作
 * @Author Tender
 * @Time 2021/8/6 0:22
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Component
public class OrderJobFactory {
    @Autowired
    public JobBuilderFactory jobBuilderFactory;

    @Autowired
    public StepBuilderFactory stepBuilderFactory;

    @Autowired
    public OrderItemProcessor processor;

    @Autowired
    private OrderJobPersistListener jobListener;

    @Autowired
    EntityManagerFactory entityManagerFactory;

    @Autowired
    JpaTransactionManager jpaTransactionManager;

    private String getQueryString() {
        StringBuilder query = new StringBuilder();

        //
        query.append("select e from Stock e ");

        return query.toString();
    }

    /**
     * Reader
     *
     * @return
     */
    private JpaCursorItemReader<Stock> createReader() {
        return new JpaCursorItemReaderBuilder<Stock>()
                .name("itemReader")
                .entityManagerFactory(entityManagerFactory)
                .queryString(getQueryString())
                .build();
    }

    /**
     * Processor
     *
     * @return
     */
    private OrderItemProcessor getProcessor() {
        return processor;
    }

    /**
     * Writer: write JobInstance to database
     *
     * @return
     */
    private JpaItemWriter<JobInstance> createWriter() {
        return new JpaItemWriterBuilder<JobInstance>()
                .entityManagerFactory(entityManagerFactory)
                .build();
    }

    /**
     * Step1
     *
     * @return
     */
    private Step step1() {
        return stepBuilderFactory.get("step1")
                .transactionManager(jpaTransactionManager)
                .<Stock, JobInstance> chunk(100)
                .reader(createReader())
                .processor(getProcessor())
                .writer(createWriter())
                .listener(new StepContextCopyListener())
                .build();
    }

    /**
     * Create Job
     *
     * @param jobId
     * @return
     */
    public Job createJob(String jobId) {
        return jobBuilderFactory.get("orderJob")
                .incrementer(new RunIdIncrementer())
                .listener(jobListener)
                .flow(step1())
                .end()
                .build();
    }

    /**
     * Create Parameters
     *
     * @param jobId
     * @param bizDate
     * @param batchNo
     * @param orderId
     * @return
     */
    public JobParameters createParameters(String jobId, String bizDate, Long batchNo, String orderId) {
        JobParameters parameters = new JobParametersBuilder()
                .addString("jobId", jobId)
                .addString("bizDate", bizDate)
                .addLong("batchNo", batchNo)
                .addString("orderId", orderId, false)
                .toJobParameters();

        return parameters;
    }
}
