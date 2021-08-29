package com.eastx.sap.bean;

import com.eastx.sap.uuid.Ipv4Snowflake;
import com.eastx.sap.uuid.Uniquer;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * @ClassName bean
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/10 0:20
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Component
public class UniquerFactory {
    @Value("${user.uuid.snowflake.dataCenterId}")
    private Long dataCenterId;

    private Uniquer object;

    public Uniquer getObject() {
        if(this.object == null) {
            this.object = new Ipv4Snowflake(dataCenterId);
        }

        return this.object;
    }
}
