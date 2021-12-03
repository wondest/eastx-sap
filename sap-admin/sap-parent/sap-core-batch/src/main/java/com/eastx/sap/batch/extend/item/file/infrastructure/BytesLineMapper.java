package com.eastx.sap.batch.extend.item.file.infrastructure;

/**
 * @ClassName BytesLineMapper
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/1 9:03
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class BytesLineMapper<T> implements LineMapper<T> {
    /**
     * 分词器
     */
    private final LineTokenizer tokenizer;

    /**
     * 映射词项
     */
    private final FieldSetMapper<T> fieldSetMapper;

    /**
     *
     * @param tokenizer
     * @param fieldSetMapper
     */
    public BytesLineMapper(LineTokenizer tokenizer, FieldSetMapper<T> fieldSetMapper) {
        this.tokenizer = tokenizer;
        this.fieldSetMapper = fieldSetMapper;
    }

    /**
     *
     * @param line
     * @param lineNumber
     * @return
     * @throws IllegalAccessException
     */
    @Override
    public T mapLine(Separator line, int lineNumber) throws ReflectiveOperationException {
        return this.fieldSetMapper.mapFieldSet(this.tokenizer.tokenize(line));
    }
}
