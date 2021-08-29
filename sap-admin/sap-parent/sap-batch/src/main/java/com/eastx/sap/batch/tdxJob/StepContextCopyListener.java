package com.eastx.sap.batch.tdxJob;

import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.listener.StepExecutionListenerSupport;
import org.springframework.batch.item.ExecutionContext;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Collectors;

/**
 * @ClassName StepExecutionContextCopyListener
 * @Description: 自动拷贝JobContext中的参数
 * @Author Tender
 * @Time 2021/8/8 23:34
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
public class StepContextCopyListener extends StepExecutionListenerSupport {
    /**
     *
     */
    private Collection<String> keys = null;

    /**
     * @param keys A list of keys corresponding to items in the
     * {@link JobParameters} that should be copied.
     */
    public void setKeys(String[] keys) {
        this.keys = Arrays.asList(keys);
    }

    @Override
    public void beforeStep(StepExecution stepExecution) {
        ExecutionContext stepContext = stepExecution.getExecutionContext();
        ExecutionContext jobContext = stepExecution.getJobExecution().getExecutionContext();

        if (keys == null) {
            keys = jobContext.entrySet().stream().map(e -> e.getKey()).collect(Collectors.toList());
        }

        for (String key : keys) {
            if (!stepContext.containsKey(key)) {
                stepContext.put(key, jobContext.get(key));
            }
        }

        //
        log.info("======= Copy keys from JobExecutionContext to StepExecutionContext =========");
        log.info(keys.toString());
        log.info("======= Copy keys from JobExecutionContext to StepExecutionContext =========");
    }
}
