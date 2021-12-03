package com.eastx.sap.batch.batchTdx;

import com.eastx.sap.batch.extend.item.file.builder.BinaryFileReaderBuilder;
import com.eastx.sap.batch.extend.item.file.core.BinaryFileItemReader;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.batch.core.configuration.annotation.JobBuilderFactory;
import org.springframework.batch.core.configuration.annotation.StepBuilderFactory;
import org.springframework.batch.core.launch.support.RunIdIncrementer;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.batch.item.file.builder.FlatFileItemWriterBuilder;
import org.springframework.batch.item.file.transform.DelimitedLineAggregator;
import org.springframework.batch.item.file.transform.PassThroughLineAggregator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.FileSystemResource;

/**
 * Spring-batch:
 *   1. declare the [step: {reader processor writer}]
 *   2. declare the listener
 *   3. assemble the job using the step and listener
 *
 * The tdx-job:
 *   1. read a tdx day file
 *   2. do some converting
 *   3. write to a csv
 *   4. update db state when the job is completed
 */
@Configuration
@EnableBatchProcessing
public class BatchTdxConfiguration {
	@Autowired
	public JobBuilderFactory jobBuilderFactory;

	@Autowired
	public StepBuilderFactory stepBuilderFactory;
	// end::setup

	// tag::[reader writer processor]
	private BinaryFileItemReader<TradeBarRO> createReader() {
		return new BinaryFileReaderBuilder<TradeBarRO>()
			.name("itemReader")
			.resource(new FileSystemResource("G:\\Program Green\\tdx_huatai\\vipdoc\\sz\\lday\\sz000001.day"))
			.targetType(TradeBarRO.class)
			.build();
	}

	private BarItemProcessor createProcessor() {
		return new BarItemProcessor();
	}

	private FlatFileItemWriter<TradeBarDO> createWriter() {
		return new FlatFileItemWriterBuilder<TradeBarDO>()
			.name("itemWriter")
			.resource(new FileSystemResource("G:\\Program Green\\sz000001.csv"))
			.lineAggregator(new DelimitedLineAggregator<TradeBarDO>())
			.build();
	}
	// end::[reader writer processor]

	// tag::job [step]
	@Bean
	public Job tdxJob(JobCompletionPersistListener listener, Step tdxStep1) {
		return jobBuilderFactory.get("tdxJob")
			.incrementer(new RunIdIncrementer())
			.listener(listener)
			.flow(tdxStep1)
			.end()
			.build();
	}

	@Bean
	public Step tdxStep1() {
		return stepBuilderFactory.get("step1")
			.<TradeBarRO, TradeBarDO> chunk(100)
			.reader(createReader())
			.processor(createProcessor())
			.writer(createWriter())
			.build();
	}
	// end::job [step]

}
