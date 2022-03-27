package com.eastx.sap.rule.builder;

import com.eastx.sap.rule.core.evaluator.Evaluator;

/**
 * @ClassName EvaluatorBuilderFactory
 * @Description:
 * @Author Tender
 * @Time 2022/3/14 8:07
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class EvaluatorBuilderFactory {
    /**
     * 创建一个构建器
     *
     * @return
     */
    public static EvaluatorBuilderFactory get() {
        return new EvaluatorBuilderFactory();
    }

    /**
     * 流式构建
     * @param evaluator
     * @return
     */
    public StreamEvaluatorBuilder stream(Evaluator evaluator) {
        StreamEvaluatorBuilder builder = new StreamEvaluatorBuilder();
        return builder.start(evaluator);
    }
}
