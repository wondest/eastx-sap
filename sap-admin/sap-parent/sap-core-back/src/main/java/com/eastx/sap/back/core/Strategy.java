package com.eastx.sap.back.core;

import com.eastx.sap.data.pojo.Bar;

import java.util.List;

/**
 * @ClassName Strategy
 * @Description: TODO
 * @Author Tender
 * @Time 2021/5/31 22:12
 * @Version 1.0
 * @Since 1.8
 **/
public interface Strategy {
    /**
     * Run the strategy using all bars once.
     */
    void evalOnce();

    /**
     * Run the strategy using a bar one by one.
     */
    void evalNext();

    /**
     * Return all bars immediately
     * @return
     */
    List<Bar> getAllBar();
}
