package com.eastx.sap.batch.tdxJob;

import com.eastx.sap.data.dao.JobBaseRepository;
import com.eastx.sap.data.dao.JobOrderRepository;
import com.eastx.sap.data.entity.JobBase;
import com.eastx.sap.data.entity.JobOrder;
import com.eastx.sap.data.type.ExecLockEnum;
import com.eastx.sap.error.ErrorEnum;
import com.eastx.sap.error.ExceptionFactory;
import com.eastx.sap.util.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.batch.core.configuration.annotation.JobScope;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

@Component
@Slf4j
@JobScope
public class OrderJobPersistListener implements JobExecutionListener {
	@Autowired
	private ExceptionFactory exceptionFactory;

	@Autowired
	JobOrderRepository jobOrderDao;

	@Autowired
	JobBaseRepository jobBaseDao;

	@Value("#{jobParameters['orderId']}")
	private String orderId;

	@Value("#{jobParameters['jobId']}")
	private String jobId;

	private void trySetRunning(String orderId, Long execId) {
		Assert.hasText(orderId, "Input orderId is e,empty");
		int count = jobOrderDao.updateWithExecState(orderId, ExecLockEnum.FREE, execId, ExecLockEnum.LOCKED);
		if(count == 0) {
			exceptionFactory.newException(ErrorEnum.FAIL, "Try to set order state failed");
		}
	}

	private void setFinish(String orderId) {
		JobOrder order =  new JobOrder();

		order.setLockState(ExecLockEnum.FREE);
		order.setId(orderId);
		order.setLastUpdated(DateUtils.getSystimestamp());

		jobOrderDao.save(order);
	}

	@Override
	public void beforeJob(JobExecution jobExecution) {
		trySetRunning(orderId, jobExecution.getJobId());

		//query job
		JobBase job = jobBaseDao.findById(jobId).orElseThrow(()->{
			return exceptionFactory.newException(ErrorEnum.FAIL, "No such job");
		});

		//set jobName
		jobExecution.getExecutionContext().put("jobName", job.getName());
	}

	@Override
	public void afterJob(JobExecution jobExecution) {
		if(jobExecution.getStatus() == BatchStatus.COMPLETED) {
			log.info("!!! JOB FINISHED! Time to verify the results");
			log.info(jobExecution.getJobParameters().getString("id"));
		}

		setFinish(orderId);
	}
}
