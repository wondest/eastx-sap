package com.eastx.sap.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.server.standard.ServerEndpointExporter;

/**
 * @ClassName WebSocketConfig
 * @Description: TODO
 * @Author Tender
 * @Time 2021/7/7 0:58
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Configuration
public class WebSocketConfiguration {
    @Bean
    public ServerEndpointExporter createServerEndpointExporter() {
        return new ServerEndpointExporter();
    }
}
