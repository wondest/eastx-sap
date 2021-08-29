package com.eastx.sap.config;

import com.eastx.sap.error.ExceptionFactory;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @ClassName ExceptionConfiguration
 * @Description: TODO
 * @Author Tender
 * @Time 2021/7/13 23:47
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Configuration
public class ExceptionConfiguration {
    @Bean(name="ExceptionFactory")
    ExceptionFactory createFactory(MessageSource i18n) {
        return new ExceptionFactory(i18n);
    }
}
