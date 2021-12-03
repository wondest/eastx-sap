package com.eastx.sap.batch.extend.item.file;

import com.eastx.sap.batch.extend.item.file.builder.BinaryFileReaderBuilder;
import com.eastx.sap.batch.extend.item.file.core.BinaryFileItemReader;
import lombok.Data;
import org.springframework.core.io.FileSystemResource;

import java.util.stream.IntStream;

/**
 * @ClassName TestExtend
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/2 22:55
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class TestExtend {

    @Data
    public static class TdxDayStructRO {
        int datetime;
        int open;
        int high;
        int low;
        int close;
        int turnover;
        int volume;
        int filler;
    }

    public static void main(String[] argv) throws Exception {
        BinaryFileItemReader<TdxDayStructRO> reader = new BinaryFileReaderBuilder<TdxDayStructRO>()
                .resource(new FileSystemResource("G:/Program Green/tdx_huatai/vipdoc/sz/lday/sz000001.day"))
                .targetType(TdxDayStructRO.class)
                .name("nothing")
                .build();

        reader.doOpen();

        TdxDayStructRO line = reader.doRead();

        IntStream.range(0,100).forEach(i->System.out.println(reader.doRead()));

        reader.doClose();

        System.out.println(Long.MAX_VALUE);
    }
}
