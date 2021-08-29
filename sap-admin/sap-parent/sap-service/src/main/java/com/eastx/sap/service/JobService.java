package com.eastx.sap.service;

import com.eastx.sap.batch.tdxJob.OrderJobFactory;
import com.eastx.sap.bean.UniquerFactory;
import com.eastx.sap.data.dao.JobBaseRepository;
import com.eastx.sap.data.dao.JobOrderRepository;
import com.eastx.sap.data.dto.JobDTO;
import com.eastx.sap.data.entity.JobBase;
import com.eastx.sap.data.entity.JobOrder;
import com.eastx.sap.data.type.JobTypeEnum;
import com.eastx.sap.error.ErrorEnum;
import com.eastx.sap.error.ExceptionFactory;
import com.eastx.sap.util.DateUtils;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @ClassName JobService
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/6 23:45
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Service
public class JobService {
    @Autowired
    private ExceptionFactory exceptionFactory;

    @Autowired
    private JobLauncher jobLauncher;

    @Autowired
    private OrderJobFactory jobFactory;

    @Autowired
    private JobBaseRepository jobBaseDao;

    @Autowired
    private JobOrderRepository jobOrderDao;

    @Autowired
    private UniquerFactory uniquerFactory;

    public JobDTO create(JobDTO dto) {
        //new
        JobBase job = new JobBase();
        job.setId(uniquerFactory.getObject().uniqueId());
        job.setName(dto.getName());
        job.setJobType(JobTypeEnum.TDX_DAY);
        job.setVersion(dto.getVersion());
        job.setLastUpdated(DateUtils.getSystimestamp());

        //process
        jobBaseDao.save(job);

        //return
        dto.setId(job.getId());
        return dto;
    }

    public void delete(String jobId) {
        //set
        JobBase job = new JobBase();
        job.setId(jobId);

        //process
        jobBaseDao.delete(job);
    }

    public void modify(JobDTO dto) {
        //new
        JobBase job = new JobBase();

        job.setId(dto.getId());
        job.setName(dto.getName());
        job.setJobType(JobTypeEnum.TDX_DAY);
        job.setLastUpdated(DateUtils.getSystimestamp());

        //process
        jobBaseDao.save(job);
    }

    /**
     *
     * @param jobId
     * @param bizDate
     * @param batchNo
     * @throws Exception
     */
    public void order(String jobId, String bizDate, Long batchNo) throws Exception {
        //query job
        JobBase job = jobBaseDao.findById(jobId)
                .orElseThrow(()-> exceptionFactory.newException(ErrorEnum.FAIL, "No such job information: ".concat(jobId)));

        //new a order
        JobOrder order = new JobOrder();
        order.setId(uniquerFactory.getObject().uniqueId());
        order.setBizDate(bizDate);
        order.setBatchNo(batchNo);
        order.setJob(job);
        order.setLastUpdated(DateUtils.getSystimestamp());
        jobOrderDao.save(order);

        //launcher job(async)
        jobLauncher.run(jobFactory.createJob(jobId)
                ,jobFactory.createParameters(jobId, bizDate, batchNo, order.getId()));
    }
}
