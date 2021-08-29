package com.eastx.sap.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

/**
 * @ClassName TaskExecutorConfiguration
 * @Description: 全局任务执行器配置
 * @Author Tender
 * @Time 2021/7/10 23:58
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Configuration
public class TaskExecutorConfiguration {
    @Bean
    public TaskExecutor threadPoolTaskExecutor() {
        ThreadPoolTaskExecutor threadPool = new ThreadPoolTaskExecutor();
        //设置核心线程数
        threadPool.setCorePoolSize(6);
        //设置最大线程数
        threadPool.setMaxPoolSize(10);
        //线程池所使用的缓冲队列
        threadPool.setQueueCapacity(10);
        //等待任务在关机时完成--表明等待所有线程执行完
        threadPool.setWaitForTasksToCompleteOnShutdown(true);
        // 等待时间 （默认为0，此时立即停止），并没等待xx秒后强制停止
        threadPool.setAwaitTerminationSeconds(60);
        // 线程名称前缀
        threadPool.setThreadNamePrefix("async-task-");
        // 初始化线程
        threadPool.initialize();

        return threadPool;
    }
}
