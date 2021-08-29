package com.eastx.sap.data.type;

import javax.persistence.AttributeConverter;
import java.util.Arrays;
import java.util.function.Function;

/**
 * @ClassName ExchangeType
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/7 15:44
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public enum ExchangeEnum {
    ShangHai(1, "sh"),
    ShenZhen(2, "sz");

    private Integer value;
    private String shortName;

    ExchangeEnum(int value, String shortName) {
        this.value = value;
        this.shortName = shortName;
    }

    public Integer intValue() {
        return value;
    }

    public String shortName() {
        return shortName;
    }

    public static class Converter extends AbstractConverter<ExchangeEnum> {
        Converter() {
            super(ExchangeEnum::intValue);
        }

        @Override
        ExchangeEnum[] allValues() {
            return ExchangeEnum.values();
        }
    }
}
