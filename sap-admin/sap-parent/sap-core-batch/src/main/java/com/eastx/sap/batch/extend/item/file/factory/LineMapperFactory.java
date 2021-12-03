package com.eastx.sap.batch.extend.item.file.factory;

import com.eastx.sap.batch.extend.item.file.infrastructure.LineMapper;

/**
 * @ClassName BytesLineFieldFactory
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/1 21:46
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface LineMapperFactory<T> {
    /**
     * 映射器
     * @return
     */
    public LineMapper<T> createLineMapper();
}
