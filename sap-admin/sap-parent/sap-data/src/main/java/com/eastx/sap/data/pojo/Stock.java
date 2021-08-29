package com.eastx.sap.data.pojo;

import lombok.Data;

import java.util.List;

/**
 * @ClassName TradeBO
 * @Description: 交易数据
 * @Author Tender
 * @Time 2021/7/11 14:11
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Data
public class Stock {
    /**
     * 代码
     */
    String code;

    /**
     * 名称
     */
    String name;

    /**
     * 交易所
     */
    String exchange;

    /**
     * 交易序列
     */
    List<Bar> series;

    /**
     * make a instance
     * @param code
     * @param series
     * @return
     */
    public static Stock valueOf(String code, List<Bar> series) {
        Stock data = new Stock();

        data.setCode(code);
        data.setSeries(series);

        return data;
    }

}
