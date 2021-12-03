package com.eastx.sap.batch.extend.item.file.factory;

import com.eastx.sap.batch.extend.item.file.infrastructure.*;
import org.springframework.util.Assert;

/**
 * @ClassName BytesLineFieldFactory
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/1 21:46
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class BytesLineFieldFactory<T> implements LineFieldFactory<T> {
    /**
     * 目标类型
     */
    private final TargetType<T> targetType;

    /**
     *
     * @param type
     */
    BytesLineFieldFactory(TargetType<T> type) {
        Assert.notNull(type, "Input type is null");
        this.targetType = type;
    }

    /**
     * 映射器
     * @return
     */
    @Override
    public FieldSetMapper<T> createFieldSetMapper() {
        return new BeanFieldSetMapper<T>(targetType);
    }

    /**
     * 分词器
     * @return
     */
    @Override
    public LineTokenizer createLineTokenizer() {
        return new FixLengthTokenizer(new DefaultFieldSetFactory(), targetType);
    }
}
