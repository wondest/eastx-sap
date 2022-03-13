package com.eastx.sap.batch.mybatisJob;

import com.eastx.sap.batch.batchTdx.TradeBarDO;
import com.eastx.sap.batch.extend.item.file.infrastructure.TargetType;
import com.eastx.sap.batch.tdxJob.OrderItemProcessor;
import com.eastx.sap.batch.tdxJob.StepContextCopyListener;
import com.eastx.sap.data.entity.JobInstance;
import com.eastx.sap.data.model.Stock;
import org.apache.ibatis.session.SqlSessionFactory;
import org.mybatis.spring.batch.MyBatisCursorItemReader;
import org.mybatis.spring.batch.builder.MyBatisCursorItemReaderBuilder;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.JobScope;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.item.ItemReader;
import org.springframework.batch.item.ItemStreamReader;
import org.springframework.batch.item.ItemWriter;
import org.springframework.batch.item.database.JpaCursorItemReader;
import org.springframework.batch.item.database.builder.JpaCursorItemReaderBuilder;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.batch.item.file.builder.FlatFileItemWriterBuilder;
import org.springframework.batch.item.file.transform.BeanWrapperFieldExtractor;
import org.springframework.batch.item.file.transform.DelimitedLineAggregator;
import org.springframework.batch.item.file.transform.LineAggregator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.core.io.FileSystemResource;
import org.springframework.jdbc.support.JdbcTransactionManager;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

/**
 * @ClassName mybatisJob
 * @Description: TODO
 * @Author Tender
 * @Time 2022/1/27 20:50
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Component
public class MybatisJobFactory {
    @Autowired
    public JobBuilderFactory jobBuilderFactory;

    @Autowired
    public StepBuilderFactory stepBuilderFactory;

    @Autowired
    SqlSessionFactory sqlSessionFactory;

    @Autowired
    public CommonItemProcessor processor;

    /**
     * Create Job
     *
     * @param mybatisStep1
     * @return
     */
    @Bean
    public Job mybatisJob(Step mybatisStep1) {
        return jobBuilderFactory.get("mybatisJob")
                .incrementer(new RunIdIncrementer())
                .flow(mybatisStep1)
                .end()
                .build();
    }

    /**
     * Step1
     *
     * @return
     */
    @Bean
    private Step mybatisStep1(ItemReader<Stock> mybatisReader, ItemWriter<StockVO> mybatisWriter) {
        return stepBuilderFactory.get("step1")
                .<Stock, StockVO> chunk(100)
                .reader(mybatisReader)
                .processor(getProcessor())
                .writer(mybatisWriter)
                .build();
    }

    private CommonItemProcessor getProcessor() {
        return processor;
    }

    /**
     * Reader
     *
     * @return
     */
    @Bean
    @StepScope
    private MyBatisCursorItemReader<Stock> mybatisReader(@Value("#{jobParameters['id']}") Long id) {
        //查询条件
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("id", id);

        return new MyBatisCursorItemReaderBuilder<Stock>()
                .sqlSessionFactory(sqlSessionFactory)
                .parameterValues(parameters)
                .queryId("com.eastx.sap.data.mapper.StockMapper.selectByPrimaryKey")
                .build();
    }

    /**
     *
     * @param outputFile
     * @return
     */
    @Bean
    @StepScope
    private FlatFileItemWriter<StockVO> mybatisWriter(@Value("#{jobParameters['outputFile']}") String outputFile) {

        TargetType<StockVO> targetType = new TargetType<StockVO>(StockVO.class);

        DelimitedLineAggregator aggregator = new DelimitedLineAggregator<StockVO>();

        BeanWrapperFieldExtractor extractor = new BeanWrapperFieldExtractor();
        extractor.setNames(targetType.getNames());

        aggregator.setDelimiter("|@|");
        aggregator.setFieldExtractor(extractor);

        return new FlatFileItemWriterBuilder<StockVO>()
                .name("itemWriter")
                .resource(new FileSystemResource(outputFile))
                .lineAggregator(aggregator)
                .lineSeparator("|@|\n")
                .build();
    }

    /**
     * Create Parameters
     *
     * @return
     */
    public JobParameters createParameters(long id, String outputFile) {
        JobParameters parameters = new JobParametersBuilder()
                .addLong("id", id)
                .addString("outputFile", outputFile, true)
                .toJobParameters();

        return parameters;
    }
}
