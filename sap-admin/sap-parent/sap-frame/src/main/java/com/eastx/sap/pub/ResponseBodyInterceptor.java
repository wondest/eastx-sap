package com.eastx.sap.pub;

import com.eastx.sap.util.JsonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.MethodParameter;
import org.springframework.http.MediaType;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;

import java.text.MessageFormat;

/**
 * @ClassName ResponseBodyInterceptor
 * @Description: TODO
 * @Author Tender
 * @Time 2021/7/14 0:19
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/

@Slf4j
@ControllerAdvice(basePackages = "com.eastx.sap.controller")
public class ResponseBodyInterceptor implements ResponseBodyAdvice {
    @Override
    public boolean supports(MethodParameter methodParameter, Class aClass) {
        return true;
    }

    @Override
    public Object beforeBodyWrite(Object body, MethodParameter methodParameter, MediaType mediaType, Class aClass, ServerHttpRequest serverHttpRequest, ServerHttpResponse serverHttpResponse) {
        if(body == null || !(body instanceof ResponseData)) {
            body = ResponseData.success(body);
        }

        try {
            log.info(MessageFormat.format("请求返回===>{0}", JsonUtils.toJson(body)));
        } catch (Exception e) {
            log.error("解析返回报文失败");
        }

        return body;
    }
}
