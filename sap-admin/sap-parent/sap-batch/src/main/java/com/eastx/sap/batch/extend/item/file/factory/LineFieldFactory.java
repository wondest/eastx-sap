package com.eastx.sap.batch.extend.item.file.factory;

import com.eastx.sap.batch.extend.item.file.infrastructure.LineTokenizer;
import com.eastx.sap.batch.extend.item.file.infrastructure.FieldSetMapper;

/**
 * @ClassName BytesLineFieldFactory
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/1 21:46
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface LineFieldFactory<T> {
    /**
     * 映射器
     * @return
     */
    FieldSetMapper<T> createFieldSetMapper();

    /**
     * 分词器
     * @return
     */
    LineTokenizer createLineTokenizer();
}
