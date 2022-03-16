package com.eastx.sap.config;

import com.eastx.sap.rule.builder.EvaluatorBuilderFactory;
import com.eastx.sap.rule.demo.DroolsMessage;
import com.eastx.sap.rule.demo.PackNameFilter;
import com.eastx.sap.rule.builder.ParameterBuilderFactory;
import com.eastx.sap.rule.core.SimpleRule;
import com.eastx.sap.rule.data.LoanFact;
import com.eastx.sap.rule.data.Parameter;
import com.eastx.sap.rule.evaluator.LoanAmtEvaluator;
import com.eastx.sap.rule.evaluator.LoanTypeEvaluator;
import com.eastx.sap.rule.processor.ThroughPassProcessor;
import org.kie.api.KieServices;
import org.kie.api.builder.KieBuilder;
import org.kie.api.builder.KieFileSystem;
import org.kie.api.builder.KieModule;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;
import org.kie.internal.io.ResourceFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.core.io.support.ResourcePatternResolver;

import java.io.IOException;
import java.util.stream.IntStream;

/**
 * @ClassName DroolsConfiguration
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/12 8:24
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Configuration
public class DroolsConfiguration {
    @Bean
    KieContainer kieContainer() throws IOException {
        KieServices ks = KieServices.Factory.get();

        ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();

        Resource[] resources = resourcePatternResolver.getResources("classpath*:" + "rules/*.drl");

        KieFileSystem kfs = ks.newKieFileSystem();

        for(Resource resource : resources) {
            kfs.write(ResourceFactory.newFileResource(resource.getFile()));
        }

        KieBuilder kb = KieServices.get().newKieBuilder(kfs);
        kb.buildAll();

        KieModule km = kb.getKieModule();


        KieContainer kc = ks.newKieContainer(km.getReleaseId());
        return kc;
    }

    /**
     * drools的一般流程
     *
     *  KieServices
     *
     *  容器加载模块：KieBuilder -> KieModule -> KieContainer
     *
     *  仓库管理模块：KieRepository <-> KieModule
     *
     *  获取新的会话：KieContainer -> KieSession
     *
     * @param argv
     */
    public static void main(String[] argv) throws IOException {
        testDrools();
        testJava();
    }

    /**
     * 核心是KieContainer
     */
    private static void commonDrools() {
        //1.获取KieServices
        KieServices kieServices = KieServices.Factory.get();
        //2.获取Kie容器对象（从
        KieContainer kieContainer = kieServices.getKieClasspathContainer();
        //3.从kie容器对象中获取会话对象
        KieSession session = kieContainer.newKieSession();
        //4.创建Fact对象（事实对象）
        Object product = new Object();
        //product.setType("diamond");
        //5.将Product对象插入到工作内存中去
        session.insert(product);
        //6.激活规则，由drools框架自动进行规则匹配，如果匹配成功，则执行规则
        session.fireAllRules();
        //7.关闭session
        session.dispose();
    }


    private static void commomStateful() throws IOException {
        KieServices ks = KieServices.Factory.get();

        ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();

        Resource[] resources = resourcePatternResolver.getResources("classpath*:" + "rules/*.drl");

        KieFileSystem kfs = ks.newKieFileSystem();

        for(Resource resource : resources) {
            kfs.write(ResourceFactory.newFileResource(resource.getFile()));
        }

        KieBuilder kb = KieServices.get().newKieBuilder(kfs);
        kb.buildAll();

        KieModule km = kb.getKieModule();


        KieContainer kc = ks.newKieContainer(km.getReleaseId());

        //3.从kie容器对象中获取会话对象
        KieSession session = kc.newKieSession();

        //4.创建Fact对象（事实对象）
        DroolsMessage message = new DroolsMessage("first", DroolsMessage.HELLO);

        message.setLoanamt(100);
        message.setLoanbal(80);

        //product.setType("diamond");
        //5.将Product对象插入到工作内存中去
        session.insert(message);
        //6.激活规则，由drools框架自动进行规则匹配，如果匹配成功，则执行规则
        session.fireAllRules(new PackNameFilter("com.ccb.abccf.cond"));
        //session.getAgenda().getAgendaGroup("cond1").setFocus();
        //session.fireAllRules();
        DroolsMessage message2 = new DroolsMessage("second", DroolsMessage.HELLO);

        message2.setLoanamt(90);
        message2.setLoanbal(100);

        session.insert(message2);

        //session.fireAllRules();
        session.fireAllRules(new PackNameFilter("com.ccb.abccf.cond"));

        //7.关闭session
        session.dispose();

        System.out.println(message);
        System.out.println(message2);
    }

    private static void testDrools() throws IOException {
        KieServices ks = KieServices.Factory.get();

        ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();

        Resource[] resources = resourcePatternResolver.getResources("classpath*:" + "rules/*.drl");

        KieFileSystem kfs = ks.newKieFileSystem();

        for(Resource resource : resources) {
            kfs.write(ResourceFactory.newFileResource(resource.getFile()));
        }

        KieBuilder kb = KieServices.get().newKieBuilder(kfs);
        kb.buildAll();

        KieModule km = kb.getKieModule();


        KieContainer kc = ks.newKieContainer(km.getReleaseId());

        //3.从kie容器对象中获取会话对象
        KieSession session = kc.newKieSession();

        //4.创建Fact对象（事实对象）
        runDroolsMany(session, 1000000);

        //7.关闭session
        session.dispose();
    }


    private static void runDroolsMany(KieSession session, int count) {

        long startTime = System.currentTimeMillis();

        IntStream.range(0, count).mapToObj(i->new DroolsMessage(String.valueOf(i), DroolsMessage.HELLO))
                .forEach(o->{
                    session.insert(o);
                    session.fireAllRules(new PackNameFilter("com.ccb.abccf.cond"));
                });

        long endTime = System.currentTimeMillis();

        System.out.println("Drools ============================");

        System.out.println("循环：" + count + "次,运行 " + (endTime-startTime) + " ms");

        System.out.println("============================");
    }


    private static void testJava() {
        runJavaMany(1000000);
    }

    private static void runJavaMany(int count) {

        //build fact - 来自业务数据
        LoanFact fact = new LoanFact();

        fact.setLoanType("test");
        fact.setLoanAmt(99);

        //build parameters
        Parameter parameter3 = ParameterBuilderFactory.get()
                .set()
                .add("test")
                .add("test2")
                .build();

        Parameter parameter1 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(50), Integer.valueOf(100))
                .build();

        Parameter parameter2 = ParameterBuilderFactory.get()
                .section()
                .between(Integer.valueOf(10), Integer.valueOf(60))
                .build();

        //build rule set
        SimpleRule rule = new SimpleRule(
                EvaluatorBuilderFactory.get().<LoanFact>stream(new LoanAmtEvaluator(parameter1))
                        .and(new LoanAmtEvaluator(parameter2))
                        .and(new LoanTypeEvaluator(parameter3))
                        .build()
                , new ThroughPassProcessor());

        long startTime = System.currentTimeMillis();

        IntStream.range(0, count).mapToObj(i->new LoanFact(100, "test"))
                .forEach(o->{
                    rule.fire(o);;
                });

        long endTime = System.currentTimeMillis();

        System.out.println("Java ============================");

        System.out.println("循环：" + count + "次,运行 " + (endTime-startTime) + " ms");

        System.out.println("============================");
    }

    private static DroolsMessage doProcess(DroolsMessage message) {
        if(message.getLoanamt() > message.getLoanbal()) {
            message.setResult(message.getMessage() + ": loanamt > loanbal");
        } else {
            message.setResult(message.getMessage() + ": loanamt <= loanbal");
        }

        return message;
    }
}
