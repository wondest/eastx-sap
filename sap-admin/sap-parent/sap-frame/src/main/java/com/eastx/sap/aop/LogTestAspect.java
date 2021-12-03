package com.eastx.sap.aop;

import com.eastx.sap.annotation.LogTest;
import lombok.extern.slf4j.Slf4j;
import net.bytebuddy.implementation.bytecode.Throw;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.*;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;

/**
 * @ClassName LogTestAspect
 * @Description: TODO
 * @Author Tender
 * @Time 2021/12/2 23:27
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/

@Aspect
@Component
@Slf4j
public class LogTestAspect {
    /**
     * 定义一个切入点: 可以定义多个
     */
    @Pointcut(value = "@annotation(com.eastx.sap.annotation.LogTest)")
    public void pointCut() {
        log.info("do logging start");
    }

    @Before(value = "pointCut()")
    public void doBefore(JoinPoint joinPoint) {
        log.info("2 - doBefore");
    }

    @Around(value = "pointCut()")
    public Object doRound(ProceedingJoinPoint joinPoint) {
        log.info("1 - Round begin");
        Object result = null;

        try {
            result = joinPoint.proceed();
        } catch (Throwable t) {
            t.printStackTrace();
        }

        log.info("5 - Round end");

        return result;
    }

    @After(value = "pointCut()")
    public void doAfter(JoinPoint joinPoint) {
        log.info("4 - doAfter");

        //获取参数值数组
        Object[] args = joinPoint.getArgs();

        //获取加强对象
        Object target = joinPoint.getTarget();

        //获取signature 该注解作用再方法上
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();

        //获取方法名
        String signatureName = signature.getName();

        //获取参数名数组
        String[] parameterNames = signature.getParameterNames();

        //获取方法的Method对象
        Method method = signature.getMethod();

        //获取返回对象
        Class returnType = method.getReturnType();

        //获取注解
        LogTest annotation = method.getDeclaredAnnotation(LogTest.class);

        log.info(annotation.value());
    }

    @AfterReturning(value = "pointCut()", returning="rtv")
    public void doAfterReturning(JoinPoint joinPoint, Integer rtv) {
        log.info("3(1) - doAfterReturning: return=" + rtv);
    }

    @AfterThrowing(value = "pointCut()")
    public void doAfterThrowing(JoinPoint joinPoint) {
        log.info("3(2) - AfterThrowing");
    }

}
