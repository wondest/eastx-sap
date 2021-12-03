package com.eastx.sap.util;

import java.sql.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @ClassName dateUtil
 * @Description: TODO
 * @Author Tender
 * @Time 2021/6/14 17:04
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class DateUtils {
    /**
     * a global timezone for strategies
     */
    private static volatile TimeZone globalTimeZone = TimeZone.getDefault();

    /**
     * The default time formatter
     */
    private static SimpleDateFormat timeFormater = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");

    /**
     * The default date formatter
     */
    private static SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * The base string
     */
    private final static String TIME_BASE_STRING = "1990-01-01";

    /**
     * The base zone
     */
    private final static String TIME_BASE_ZONE = "GMT+0";

    /**
     * The base long
     */
    private final static long TIME_BASE_LONG = getSafeNum(TIME_BASE_STRING, TimeZone.getTimeZone(TIME_BASE_ZONE));

    /**
     * Set the timezone
     */
    public static void setTimeZone(TimeZone zone) {
        globalTimeZone = checkNotNull(zone, "Input parameter %s should not be null.", "zone");
        dateFormatter.setTimeZone(zone);
        timeFormater.setTimeZone(zone);
    }

    /**
     * Get the timezone
     * @return
     */
    public static TimeZone getTimeZone() {
        return globalTimeZone;
    }

    /**
     * Parse the string to long with the default formatter
     * @param dateStr
     * @return
     */
    public static long parse(String dateStr) throws ParseException {
        return relative(dateFormatter.parse(dateStr).getTime());
    }

    /**
     * Parse the string to long with a formatter
     * @param dateStr
     * @return
     */
    public static long parse(String dateStr, SimpleDateFormat formatter) throws ParseException {
        formatter.setTimeZone(globalTimeZone);
        return relative(formatter.parse(dateStr).getTime());
    }

    /**
     * Get the relative long
     */
    private static long relative(long absDateNum) {
        return absDateNum - TIME_BASE_LONG;
    }

    /**
     * Get the absolute long
     */
    private static long absolute(long relaDateNum) {
        return relaDateNum + TIME_BASE_LONG;
    }

    /**
     * Get long
     * @param dateStr
     * @return
     */
    private static long getSafeNum(String dateStr, TimeZone zone) {
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
        formatter.setTimeZone(zone);
        long dateNum = 0;
        try {
            dateNum = formatter.parse(dateStr).getTime();
        } catch (ParseException e) {
            e.printStackTrace();
            dateNum = 0;
        } finally {
            return dateNum;
        }
    }

    /**
     * Convert long to string
     * @param dateNum
     * @return
     */
    public static String num2Str(long dateNum) {
        return dateFormatter.format(num2date(dateNum));
    }

    /**
     * Convert long to date
     * @param dateNum
     * @return
     */
    public static Date num2date(long dateNum) {
        return new Date(absolute(dateNum));
    }

    /**
     * Convert date to long
     * @param date
     * @return
     */
    public static long date2num(Date date) {
        return relative(date.getTime());
    }

    /**
     * Get system's timestamp
     * @return
     */
    public static Timestamp getSystimestamp() {
        return new Timestamp(System.currentTimeMillis());
    }

    /**
     * Test main
     * @param argv
     * @throws ParseException
     */
    public static void main(String[] argv) throws ParseException {
        String fa = "yyyy-MM-dd HH:mm:ss";
        String sa = "1970-01-01 00:00:00.000";
        String sb = "1998-03-02 12:20:00.000";
        String za = "GMT+0";
        String zb = "GMT+8";

        DateUtils.setTimeZone(TimeZone.getTimeZone(za));
        long lb0 = DateUtils.parse(sb, DateUtils.timeFormater);

        System.out.println(DateUtils.num2Str(lb0));
        System.out.println(DateUtils.TIME_BASE_LONG);

        DateUtils.setTimeZone(TimeZone.getTimeZone(zb));
        long lb8 = DateUtils.parse(sb, DateUtils.timeFormater);

        System.out.println(String.format("date=%s lb1(%s)=%d lb2(%s)=%d", sa, za, lb0, zb, lb8));

    }
}
