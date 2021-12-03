package com.eastx.sap.back.core.data.source;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.function.Consumer;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @ClassName FlatFileDataSource
 * @Description: TODO
 * @Author Tender
 * @Time 2021/5/29 21:46
 * @Version 1.0
 * @Since 1.8
 **/
public class FlatFileDataSource extends AbstractDataSource.Persist {
    /**
     * 源文件
     */
    private final String fileName;

    /**
     * 文件读取对象
     */
    private BufferedReader reader;

    public FlatFileDataSource(String fileName) {
        this.fileName = fileName;
    }

    @Override
    public void start() {
        open();
    }

    @Override
    public void stop() {
        close();
    }

    /**
     * Open the reader
     */
    private void open() {
        //Open the reader
        if(null == reader) {
            try {
                reader = new BufferedReader(new FileReader(fileName));
            } catch (FileNotFoundException e) {
                e.printStackTrace();
                throw new RuntimeException("start failed!");
            }
        }
    }

    /**
     * Close the reader
     */
    private void close() {
        if(null != reader) {
            try {
                reader.close();
            } catch (IOException e) {
                e.printStackTrace();
                throw new RuntimeException("stop failed");
            }
        }
    }

    /**
     * Ensure the reader is opening
     */
    private void ensureOpen() {
        checkNotNull(reader, "source is closed");
    }

    /**
     * Process all
     * @return
     */
    private void process(Consumer feed) {
        String data;
        ensureOpen();

        try {
            while (null != (data = reader.readLine())) {
                feed.accept(data);
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException("read failed");
        }
    }

    @Override
    public void forAll(Consumer feed) {
        start();
        process(feed);
        stop();
    }
}
