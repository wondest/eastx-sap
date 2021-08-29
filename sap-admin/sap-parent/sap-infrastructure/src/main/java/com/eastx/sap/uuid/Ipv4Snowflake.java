package com.eastx.sap.uuid;

import cn.hutool.core.net.NetUtil;
import cn.hutool.core.util.IdUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import cn.hutool.core.lang.Snowflake;
import org.springframework.util.Assert;

/**
 * @ClassName LocalSnowflake
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/9 0:23
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class Ipv4Snowflake implements Uniquer{
    /**
     *
     */
    private Long dataCenterId;

    private Long workerId;

    private Snowflake holder;

    public Ipv4Snowflake(Long dataCenterId) {
        Assert.notNull(dataCenterId, "");
        this.dataCenterId = dataCenterId;
        this.workerId = 1L;
        this.holder = IdUtil.getSnowflake(this.workerId, this.dataCenterId);
    }

    /**
     * Long.MAX_VALUE = 9223372036854775807 && length(9223372036854775807) = 19
     * @return
     */
    @Override
    public String uniqueId() {
        return String.format("%020d", this.holder.nextId());
    }

    // for 循环每次id不一样
    public static long snowflake(){
        Snowflake snowflake = IdUtil.getSnowflake(32, 1);
        long id = snowflake.nextId();
        return id;
    }

    public static void main(String[] args) {
        System.out.println(NetUtil.getLocalhost().getAddress()[3]);
        System.out.println(snowflake());
    }
}
