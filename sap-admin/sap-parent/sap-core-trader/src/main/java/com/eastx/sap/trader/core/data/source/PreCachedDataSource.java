package com.eastx.sap.trader.core.data.source;

import com.eastx.sap.util.DateUtils;
import lombok.Data;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.Consumer;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @ClassName PreCachedDataSource
 * @Description: 将数据一次性读入内存
 * @Author Tender
 * @Time 2021/5/29 21:46
 * @Version 1.0
 * @Since 1.8
 **/
public class PreCachedDataSource extends AbstractDataSource.Persist {
    /**
     * 源文件
     */
    private final String fileName;

    /**
     * 文件读取对象
     */
    private BufferedReader reader;

    /**
     *
     */
    private List<CachedElement> cachedList;

    /**
     * Table titles.
     */
    private final int ignored;

    /**
     * The number of row processed.
     */
    private int processed = 0;

    /**
     *
     */
    @Data
    private static class CachedElement {
        private long sorter;
        private Object data;

        public static CachedElement valueOf(long sorter, Object data) {
            CachedElement obj = new CachedElement();
            obj.setData(data);
            obj.setSorter(sorter);
            return obj;
        }
    }

    public PreCachedDataSource(String fileName, int ignored) {
        this.fileName = fileName;
        this.cachedList = new ArrayList<CachedElement>();
        this.ignored = (ignored > 0)?ignored:0;
    }

    @Override
    public void start() {
        String data;

        open();
        ensureOpen();

        try {
            while (null != (data = reader.readLine())) {
                if(ignored <= processed) {
                    String[] fields = data.split(",");
                    cachedList.add(CachedElement.valueOf(DateUtils.parse(fields[0]), data));
                }

                processed++;
            }
        } catch (ParseException e) {
            e.printStackTrace();
            throw new RuntimeException(String.format("日期字符串非法"));
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException("读取文件失败");
        }

        close();
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
        cachedList.stream().sorted(Comparator.comparingLong(CachedElement::getSorter)).forEach(e->feed.accept(e.getData()));
    }

    @Override
    public void forAll(Consumer feed) {
        start();
        process(feed);
        stop();
    }
}
