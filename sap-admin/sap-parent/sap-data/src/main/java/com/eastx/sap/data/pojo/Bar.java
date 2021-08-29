package com.eastx.sap.data.pojo;

import lombok.Data;

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
public class Bar {
    /**
     *  时间序列
     */
    private String datetime;

    /**
     *  交易信号
     */
    private Signal signal;

    /**
     *  交易价格
     */
    private Price price;

    /**
     *  交易量
     */
    private Double volume;

    /**
     *
     * @param signal
     * @param price
     * @return
     */
    public static Bar valueOf(String datetime, Signal signal, Price price, Double volume) {

        Bar data = new Bar();

        data.setDatetime(datetime);
        data.setSignal(signal);
        data.setPrice(price);
        data.setVolume(volume);

        return data;
    }
}
