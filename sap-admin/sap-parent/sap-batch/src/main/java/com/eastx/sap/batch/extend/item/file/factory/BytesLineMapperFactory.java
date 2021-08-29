package com.eastx.sap.batch.extend.item.file.factory;

import com.eastx.sap.batch.extend.item.file.infrastructure.BytesLineMapper;
import com.eastx.sap.batch.extend.item.file.infrastructure.LineMapper;
import com.eastx.sap.batch.extend.item.file.infrastructure.TargetType;

/**
 * @ClassName BytesLineMapperFactory
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/1 21:52
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class BytesLineMapperFactory<T> implements LineMapperFactory<T> {
    /**
     *
     */
    private LineFieldFactory<T> factory;

    public BytesLineMapperFactory(TargetType type) {
        this.factory = new BytesLineFieldFactory<T>(type);
    }

    @Override
    public LineMapper<T> createLineMapper() {
        return new BytesLineMapper<T>(factory.createLineTokenizer(), factory.createFieldSetMapper());
    }
}
