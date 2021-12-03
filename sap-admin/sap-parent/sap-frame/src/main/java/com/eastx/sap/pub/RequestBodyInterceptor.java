package com.eastx.sap.pub;

import com.eastx.sap.util.JsonUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.MethodParameter;
import org.springframework.http.HttpInputMessage;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.mvc.method.annotation.RequestBodyAdvice;

import java.io.IOException;
import java.lang.reflect.Type;
import java.text.MessageFormat;
import java.util.Optional;

/**
 * @ClassName Advice
 * @Description: TODO
 * @Author Tender
 * @Time 2021/7/14 0:19
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/

@Slf4j
@ControllerAdvice(basePackages = "com.eastx.sap.controller")
public class RequestBodyInterceptor implements RequestBodyAdvice {
    @Override
    public boolean supports(MethodParameter methodParameter, Type type, Class<? extends HttpMessageConverter<?>> aClass) {
        return true;
    }

    @Override
    public HttpInputMessage beforeBodyRead(HttpInputMessage httpInputMessage, MethodParameter methodParameter, Type type, Class<? extends HttpMessageConverter<?>> aClass) throws IOException {
        return httpInputMessage;
    }

    @Override
    public Object afterBodyRead(Object body, HttpInputMessage httpInputMessage, MethodParameter methodParameter, Type type, Class<? extends HttpMessageConverter<?>> aClass) {
        try {
            RequestMapping requestMapping = methodParameter.getMethodAnnotation(RequestMapping.class);
            log.debug(MessageFormat.format("请求地址=====>{0}", Optional.ofNullable(requestMapping.value()[0]).orElse("")));
            log.debug(MessageFormat.format("请求参数=====>{0}", JsonUtils.toJson(body)));
        } catch (Exception e) {
            log.error("解析请求报文失败");
        }
        return body;
    }

    @Override
    public Object handleEmptyBody(Object body, HttpInputMessage httpInputMessage, MethodParameter methodParameter, Type type, Class<? extends HttpMessageConverter<?>> aClass) {
        RequestMapping requestMapping = methodParameter.getMethodAnnotation(RequestMapping.class);
        log.debug(MessageFormat.format("请求地址=====>{0}", Optional.ofNullable(requestMapping.value()[0]).orElse("")));
        return body;
    }
}
