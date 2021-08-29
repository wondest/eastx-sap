package com.eastx.sap.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;

/**
 * @ClassName AsyncTaskConfiguration
 * @Description: 全局异步任务配置
 * @Author Tender
 * @Time 2021/7/10 23:58
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Configuration
public class AsyncTaskConfiguration implements AsyncConfigurer {
    @Autowired
    TaskExecutor taskExecutor;

    @Override
    @Bean
    public Executor getAsyncExecutor() {
        return taskExecutor;
    }
}
