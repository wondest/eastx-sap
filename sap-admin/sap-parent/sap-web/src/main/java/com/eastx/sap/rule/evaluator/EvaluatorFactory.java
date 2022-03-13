package com.eastx.sap.rule.evaluator;

/**
 * @ClassName EvaluatorSupport
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/13 15:44
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class EvaluatorFactory {

    /**
     *
     * @param <F>
     * @return
     */
    public static <F> EvaluatorStream<F> stream() {
        return new CompositeEvaluator<>();
    }



}
