package com.eastx.sap.data.pojo;

import lombok.Data;

/**
 * @ClassName StockTradeBO
 * @Description: 交易价格
 * @Author Tender
 * @Time 2021/7/11 14:11
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Data
public class Price {
    /**
     * The open price
     */
    private Double open;

    /**
     * The highest price
     */
    private Double high;

    /**
     * The lowest price
     */
    private Double low;

    /**
     * The close price
     */
    private Double close;

    public static Price valueOf(Double open, Double high, Double low, Double close) {
        Price data = new Price();

        data.setOpen(open);
        data.setHigh(high);
        data.setLow(low);
        data.setClose(close);

        return data;
    }
}
