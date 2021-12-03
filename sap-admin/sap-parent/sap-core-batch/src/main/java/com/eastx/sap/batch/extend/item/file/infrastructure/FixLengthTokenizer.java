package com.eastx.sap.batch.extend.item.file.infrastructure;

import org.springframework.util.Assert;
import java.util.Arrays;

/**
 * @ClassName FixLengthTokenizer
 * @Description: 固定长度分词器
 * @Author Tender
 * @Time 2021/8/1 17:54
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class FixLengthTokenizer implements LineTokenizer {
    /**
     * Use the factory to create a new fieldSet.
     */
    private FieldSetFactory fieldSetFactory;

    /**
     * The ranges of the fields,include upper and lower
     */
    private Range[] ranges;

    /**
     * The names of the fields
     */
    private String[] names;

    /**
     *
     * @param factory
     * @param type
     */
    public FixLengthTokenizer(FieldSetFactory factory, final TargetType type) {
        Assert.notNull(factory, "Input factory should not be null");
        Assert.notNull(type, "Input type should not be null");

        this.fieldSetFactory = factory;
        this.ranges = type.getRanges();
        this.names = type.getNames();
    }

    @Override
    public FieldSet tokenize(Separator line) {
        return this.fieldSetFactory.create(names, doTokenize(line));
    }

    /**
     *
     * @param line
     * @return
     */
    protected Separator[] doTokenize(Separator line) {
        return Arrays.stream(ranges)
                .map(r -> line.slice(r.getLower(), r.getUpper()))
                .toArray(Separator[]::new);
    }
}
