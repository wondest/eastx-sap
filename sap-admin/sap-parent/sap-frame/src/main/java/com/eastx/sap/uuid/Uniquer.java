package com.eastx.sap.uuid;

import cn.hutool.core.lang.Snowflake;
import cn.hutool.core.net.NetUtil;
import cn.hutool.core.util.IdUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * @ClassName Uniquer
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/9 0:23
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Component
public interface Uniquer {
    String uniqueId();
}
