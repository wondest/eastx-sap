package com.eastx.sap.data.pojo;

import lombok.Data;

import java.util.Optional;

/**
 * @ClassName SignalDO
 * @Description: 交易信号
 * @Author Tender
 * @Time 2021/7/11 14:11
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Data
public class Signal {
    /**
     * 交易方向: 买入
     */
    private static final int INT_DIRECTION_BUY = 1;

    /**
     * 交易方向: 卖出
     */
    private static final int INT_DIRECTION_SELL = -1;

    /**
     * 交易方向: 维持
     */
    private static final int INT_DIRECTION_NONE = 0;

    /**
     * 交易方向: 买入
     */
    private static final String STRING_DIRECTION_BUY = "B";

    /**
     * 交易方向: 卖出
     */
    private static final String STRING_DIRECTION_SELL = "S";

    /**
     * 交易方向: 维持
     */
    private static final String STRING_DIRECTION_NONE = "N";

    /**
     *  B - Buy     1
     *  S - Sell   -1
     *  N - None    0
     */
    private String direction;

    /**
     * The amount of the operation
     */
    private Double amount;

    /**
     *
     * @param direction
     * @param amount
     * @return
     */
    private static Signal valueOf(String direction, double amount) {
        Signal data = new Signal();

        data.setDirection(direction);
        data.setAmount(amount);

        return data;
    }

    /**
     *
     * @param direction
     * @param amount
     * @return
     */
    public static Signal valueOf(Double direction, double amount) {
        return Signal.valueOf(Optional.ofNullable(direction).map(Signal::toStringDirection).orElse(STRING_DIRECTION_NONE), amount);
    }

    /**
     * from double direction to string direction
     * @param direction
     * @return
     */
    private static String toStringDirection(Double direction) {
        switch(direction.intValue()) {
            case INT_DIRECTION_BUY:
                return "B";
            case INT_DIRECTION_SELL:
                return "S";
            case INT_DIRECTION_NONE:
            default:
                return "N";
        }
    }

    /**
     *
     * @param amount
     * @return
     */
    public static Signal buyOf(double amount) {
        return Signal.valueOf(STRING_DIRECTION_BUY, amount);
    }

    /**
     *
     * @param amount
     * @return
     */
    public static Signal sellOf(double amount) {
        return Signal.valueOf(STRING_DIRECTION_SELL, amount);
    }

    /**
     *
     * @param amount
     * @return
     */
    public static Signal noneOf(double amount) {
        return Signal.valueOf(STRING_DIRECTION_NONE, amount);
    }

    /**
     *
     * @param amount
     * @return
     */
    public static Signal randOf(double amount) {
        return Signal.valueOf(Optional.of(Math.random()).map(r -> {
            if(r < 0.03) {
                return STRING_DIRECTION_SELL;
            } else if (r > 0.98) {
                return STRING_DIRECTION_BUY;
            } else {
                return STRING_DIRECTION_NONE;
            }
        }).get(), amount);
    }
}
