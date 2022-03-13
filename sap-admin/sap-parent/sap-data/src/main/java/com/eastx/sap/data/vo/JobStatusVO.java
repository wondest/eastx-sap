package com.eastx.sap.data.vo;

import lombok.Data;

/**
 * @ClassName JobStatusVI
 * @Description: TODO
 * @Author Tender
 * @Time 2022/2/19 18:26
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Data
public class JobStatusVO {

    public static final String RUNNING = "10";
    public static final String ALREADY_RUNNING = "11";
    public static final String COMPLETED = "20";
    public static final String ALREADY_COMPLETED = "21";
    public static final String FAILED = "90";

    String  jobStatus;
    Long jobExecutionId;
    Long jobInstanceId;
}
