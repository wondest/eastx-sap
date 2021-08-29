package com.eastx.sap.batch.batchTdx;

import com.eastx.sap.batch.extend.item.file.builder.BinaryFileReaderBuilder;
import com.eastx.sap.batch.extend.item.file.core.BinaryFileItemReader;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.batch.item.file.builder.FlatFileItemWriterBuilder;
import org.springframework.batch.item.file.transform.DelimitedLineAggregator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.FileSystemResource;
import org.springframework.stereotype.Component;

import java.util.Date;

/**
 * @ClassName TdxJobBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/6 0:22
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Component
public class TdxJobFactory {
    @Autowired
    public JobBuilderFactory jobBuilderFactory;

    @Autowired
    public StepBuilderFactory stepBuilderFactory;

    @Autowired
    public TdxContext context;

    @Autowired
    public BarItemProcessor processor;

    private BinaryFileItemReader<TradeBarRO> createReader(String inputFile) {
        return new BinaryFileReaderBuilder<TradeBarRO>()
                .name("itemReader")
                .resource(new FileSystemResource(inputFile))
                .targetType(TradeBarRO.class)
                .build();
    }

    private BarItemProcessor getProcessor() {
        return processor;
    }

    private FlatFileItemWriter<TradeBarDO> createWriter(String outputFile) {
        return new FlatFileItemWriterBuilder<TradeBarDO>()
                .name("itemWriter")
                .resource(new FileSystemResource(outputFile))
                .lineAggregator(new DelimitedLineAggregator<TradeBarDO>())
                .build();
    }

    private Step step1(String inputFile, String outputFile) {
        return stepBuilderFactory.get("step1")
                .<TradeBarRO, TradeBarDO> chunk(100)
                .reader(createReader(inputFile))
                .processor(getProcessor())
                .writer(createWriter(outputFile))
                .build();
    }

    public Job createJob(String code) {
        String pairFile[] = context.getPairFile(code);

        return jobBuilderFactory.get("tdxJob")
                .incrementer(new RunIdIncrementer())
                .flow(step1(pairFile[0], pairFile[1]))
                .end()
                .build();
    }

    public JobParameters createParameters(String code, Date bizDate) {
        JobParameters parameters = new JobParametersBuilder()
                .addDate("bizDate", bizDate)
                .addString("code", code)
                .toJobParameters();
        return parameters;
    }
}
