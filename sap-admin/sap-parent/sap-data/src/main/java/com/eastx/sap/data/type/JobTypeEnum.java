package com.eastx.sap.data.type;

/**
 * @ClassName JobTypeEnum
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/7 15:44
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public enum JobTypeEnum {
    TDX_DAY(1, "tdxDay");

    private Integer value;
    private String shortName;

    JobTypeEnum(int value, String shortName) {
        this.value = value;
        this.shortName = shortName;
    }

    public Integer intValue() {
        return value;
    }

    public String shortName() {
        return shortName;
    }

    public static class Converter extends AbstractConverter<JobTypeEnum> {
        Converter() {
            super(JobTypeEnum::intValue);
        }

        @Override
        JobTypeEnum[] allValues() {
            return JobTypeEnum.values();
        }
    }
}
