package com.eastx.sap.batch.demo;

import cn.hutool.core.lang.Assert;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.stereotype.Component;

/**
 * @ClassName SleepTasklet
 * @Description: TODO
 * @Author Tender
 * @Time 2022/2/19 18:06
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
@Component
@StepScope
public class DemoSleepTasklet implements Tasklet {

    private int counter = 10;

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {

        int current = 0;

        while(current < counter) {
            log.info("Sleeping --- execute:" + counter);
            Thread.sleep(1000);
            current++;
        }

        return RepeatStatus.FINISHED;
    }
}
