package com.eastx.sap.batch.tdxJob;

import com.eastx.sap.data.entity.JobBase;
import com.eastx.sap.data.entity.JobInstance;
import com.eastx.sap.data.model.Stock;
import com.eastx.sap.util.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * 这是SpringBatch的一个后绑定技术，就是在生成Step的时候，才去创建bean，因为这个时候jobParameter才传过来。\
 * 如果加载配置信息的时候就创建bean，这个时候jobParameter的值还没有产生，会抛出异常。
 */
@Slf4j
@Component
@StepScope
public class OrderItemProcessor implements ItemProcessor<Stock, JobInstance> {
	@Value("#{jobParameters['code']}")
	private String code;

	@Value("#{jobParameters['bizDate']}")
	private String bizDate;

	@Value("#{jobParameters['batchNo']}")
	private Long batchNo;

	@Value("#{jobParameters['jobName']}")
	private String jobName;

	@Value("#{jobParameters['jobId']}")
	private String jobId;

	@Value("#{stepExecution.executionContext}")
	private ExecutionContext ecStep;

	@Override
	public JobInstance process(final Stock stock) {
		//get bizKey, like sh000001
		String bizKey = stock.getExchange().toString().concat(stock.getCode());

		//new a jobInstance
		JobInstance instance = new JobInstance();

		instance.setBizDate(bizDate);
		instance.setBizKey(bizKey);
		instance.setName(getInstanceName(bizKey));
		instance.setLastUpdated(DateUtils.getSystimestamp());

		JobBase job = new JobBase();
		job.setId(jobId);
		instance.setJob(job);

		log.info("Get parameter from stepContext: jobId=" + ecStep.getLong("jobId"));
		log.info("Get parameter from stepContext: jobName=" + ecStep.getString("jobName"));

		//return a jobInstance
		return instance;
	}

	/**
	 *
	 * @return
	 */
	private String getInstanceName(String bizKey) {
		return new StringBuilder(jobName)
				.append("_").append(bizKey.toUpperCase())
				.append("_").append(this.bizDate)
				.toString();
	}

}
