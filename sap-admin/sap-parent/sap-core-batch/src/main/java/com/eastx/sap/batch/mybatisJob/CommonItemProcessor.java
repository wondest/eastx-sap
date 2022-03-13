package com.eastx.sap.batch.mybatisJob;

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
 * @ClassName CommonItemProcessor
 * @Description: TODO
 * @Author Tender
 * @Time 2022/1/27 20:58
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
@Component
@StepScope
public class CommonItemProcessor implements ItemProcessor<Stock, StockVO>  {
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
    public StockVO process(final Stock stock) {
        StockVO out = new StockVO();

        out.setId(stock.getId().toString());
        out.setLine(new StringBuilder().append(stock.getCode()).append(stock.getExchange()).toString());

        //return a jobInstance
        return out;
    }
}
