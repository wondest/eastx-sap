package com.eastx.sap.controller;

import com.eastx.sap.data.dto.JobDTO;
import com.eastx.sap.data.request.JobRequest;
import com.eastx.sap.data.request.JobResponse;
import com.eastx.sap.error.ExceptionFactory;
import com.eastx.sap.service.JobService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.configuration.annotation.EnableBatchProcessing;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.Assert;
import org.springframework.web.bind.annotation.*;

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
@RequestMapping("job")
public class JobController {
    @Autowired
    JobService jobService;

    @RequestMapping(value="/{id}/order", method= RequestMethod.GET)
    public void order(@PathVariable(name="id") String jobId, @RequestParam String bizDate, @RequestParam Long batchNo) throws Exception {
        jobService.order(jobId, bizDate, batchNo);
    }

    @PutMapping(value="/{id}")
    public void modify(@PathVariable(name="id") String jobId, @RequestBody JobRequest request) {
        JobDTO dto = checkModify(request);
        dto.setId(jobId);
        jobService.modify(dto);
    }

    @DeleteMapping(value="/{id}")
    public void remove(@PathVariable(name="id") String jobId) {
        Assert.hasText(jobId, "The request jobId is empty");
        jobService.delete(jobId);
    }

    @PostMapping
    public JobResponse create(@RequestBody JobRequest request) {
        JobDTO dto = checkCreate(request);
        return getResponse(jobService.create(dto));
    }

    /**
     *
     * @param request
     * @return
     */
    private JobDTO checkModify(JobRequest request) {
        Assert.notNull(request, "The request is null");

        JobDTO dto = new JobDTO();

        dto.setName(request.getName());
        dto.setVersion(request.getVersion());

        return dto;
    }

    /**
     *
     * @param request
     * @return
     */
    private JobDTO checkCreate(JobRequest request) {
        Assert.notNull(request, "The request is null");
        Assert.hasText(request.getName(), "The request.name is illegal");

        JobDTO dto = new JobDTO();
        dto.setName(request.getName());
        dto.setVersion(request.getVersion());

        return dto;
    }

    /**
     *
     * @param dto
     * @return
     */
    private JobResponse getResponse(JobDTO dto) {
        JobResponse response = new JobResponse();
        response.setId(dto.getId());
        return response;
    }
}
