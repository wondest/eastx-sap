package com.eastx.sap.data.type;

/**
 * @ClassName ExchangeType
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/7 15:44
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public enum ExecLockEnum {
    FREE(1),
    LOCKED(2);

    private Integer value;

    ExecLockEnum(int value) {
        this.value = value;
    }

    public Integer intValue() {
        return value;
    }

    public static class Converter extends AbstractConverter<ExecLockEnum> {
        Converter() {
            super(ExecLockEnum::intValue);
        }

        @Override
        ExecLockEnum[] allValues() {
            return ExecLockEnum.values();
        }
    }
}
