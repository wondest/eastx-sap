package com.eastx.sap.batch.extend.item.file.builder;

import com.eastx.sap.batch.extend.item.file.core.BinaryFileItemReader;
import com.eastx.sap.batch.extend.item.file.factory.BytesLineMapperFactory;
import com.eastx.sap.batch.extend.item.file.infrastructure.TargetType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.item.file.builder.FlatFileItemReaderBuilder;
import org.springframework.core.io.Resource;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * @ClassName BinaryFileReaderBuilder
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/1 21:15
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
public class BinaryFileReaderBuilder<T> {
    /**
     *
     */
    private Resource resource;

    /**
     *
     */
    private int linesToSkip = 0;

    /**
     *
     */
    private TargetType<T> targetType;

    /**
     * 使用ExecutionContext保存状态
     */
    private boolean saveState = true;

    /**
     * 使用ExecutionContext保存状态,name是key值
     */
    private String name;


    public BinaryFileReaderBuilder<T> resource(Resource resource) {
        this.resource = resource;
        return this;
    }


    public BinaryFileReaderBuilder<T> linesToSkip(int linesToSkip) {
        this.linesToSkip = linesToSkip;
        return this;
    }

    public BinaryFileReaderBuilder<T> targetType(Class<T> type) {
        this.targetType = new TargetType(type);
        return this;
    }

    /**
     * Configure if the state of the {@link org.springframework.batch.item.ItemStreamSupport}
     * should be persisted within the {@link org.springframework.batch.item.ExecutionContext}
     * for restart purposes.
     *
     * @param saveState defaults to true
     * @return The current instance of the builder.
     */
    public BinaryFileReaderBuilder<T> saveState(boolean saveState) {
        this.saveState = saveState;

        return this;
    }

    /**
     * The name used to calculate the key within the
     * {@link org.springframework.batch.item.ExecutionContext}. Required if
     * {@link #saveState(boolean)} is set to true.
     *
     * @param name name of the reader instance
     * @return The current instance of the builder.
     * @see org.springframework.batch.item.ItemStreamSupport#setName(String)
     */
    public BinaryFileReaderBuilder<T> name(String name) {
        this.name = name;
        return this;
    }

    /**
     *
     * @return
     */
    public BinaryFileItemReader<T> build() {
        if(this.saveState) {
            Assert.state(StringUtils.hasText(this.name),
                    "The name is required when saveState is set to true.");
        }

        Assert.notNull(resource, "The resource should not be null");
        Assert.isTrue(linesToSkip >= 0, "linesToSkip should not be negative");
        Assert.notNull(targetType, "The targetType should not be null");

        //create a reader
        BinaryFileItemReader<T> reader = new BinaryFileItemReader<T>(targetType.getChunkSize());

        //set a name
        reader.setName(this.name);

        //set a resource
        reader.setResource(resource);

        //set something..
        reader.setLinesToSkip(linesToSkip);

        //set a lineMapper
        BytesLineMapperFactory<T> factory = new BytesLineMapperFactory<T>(targetType);
        reader.setLineMapper(factory.createLineMapper());

        return reader;
    }
}
