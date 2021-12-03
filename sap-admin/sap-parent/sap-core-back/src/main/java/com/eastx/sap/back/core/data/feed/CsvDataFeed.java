package com.eastx.sap.back.core.data.feed;

import com.eastx.sap.util.DateUtils;
import com.eastx.sap.back.core.buffer.BoxDouble;
import lombok.extern.slf4j.Slf4j;

import java.text.ParseException;
import java.text.SimpleDateFormat;

/**
 * @ClassName CsvDataFeed
 * @Description: TODO
 * @Author Tender
 * @Time 2021/5/30 22:44
 * @Version 1.0
 * @Since 1.8
 **/
@Slf4j
public class CsvDataFeed extends AbstractDataSeries implements DataFeed {
    /**
     * Table titles.
     */
    private final int headers;

    /**
     * Row fields separator.
     */
    private final String separator;

    /**
     * Date format.
     */
    private final static SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * The number of row processed.
     */
    private int processed = 0;

    /**
     * The number of row processed.
     */
    private int errors = 0;

    /**
     *
     */
    private static int TOO_MANY_ERRORS = 100;

    public CsvDataFeed(int headers, String separator) {
        this.headers = (headers > 0)?headers:0;
        this.separator = separator;
    }

    public CsvDataFeed(int headers) {
        this(headers, ",");
    }

    /**
     * Process a bar.
     * @param oneBar
     * @return
     */
    private boolean process(Object oneBar) {
        String[] fields = map(oneBar).split(separator);

        try {
            //add datetime
            datetime().append(BoxDouble.dateOf(DateUtils.parse(fields[0], formatter)));

            //add price
            open().append(BoxDouble.valueOf(fields[1]));
            high().append(BoxDouble.valueOf(fields[2]));
            low().append(BoxDouble.valueOf(fields[3]));
            close().append(BoxDouble.valueOf(fields[4]));

            //add volume
            volume().append(BoxDouble.valueOf(fields[5]));

            return true;
        } catch (ParseException e) {
            errors++;
            log.error(String.format("日期格式非法:{processed=%d}{bar=%s}", processed, oneBar), e);
        }

        return false;
    }

    /**
     *
     * @return
     */
    private boolean ignoreError() {
        return (errors <= TOO_MANY_ERRORS);
    }

    /**
     *
     * @return
     */
    private boolean ignoreHeader() {
        return (processed++) < headers;
    }

    /**
     * Map the raw to inner data structure.
     * @param raw
     * @return
     */
    private String map(Object raw) {
        return (String)raw;
    }

    /**
     * Consume a bar.
     * @param oneBar
     * @return
     */
    @Override
    public boolean accept(Object oneBar) {
        if(!ignoreError()) {
            log.debug(String.format("错误记录数量超过上限:errors=%d limit=%d", errors, TOO_MANY_ERRORS));
            return false;
        } else if(ignoreHeader()) {
            log.debug(String.format("跳过标题内容:processed=%d headers=%d", processed, headers));
            return false;
        } else {
            return process(oneBar);
        }
    }

    @Override
    public DataSeries getData() {
        return this;
    }
}
