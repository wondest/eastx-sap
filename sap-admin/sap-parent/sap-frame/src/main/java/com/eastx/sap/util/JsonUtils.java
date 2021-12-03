package com.eastx.sap.util;

import com.google.gson.Gson;

/**
 * @ClassName JsonUtil
 * @Description: TODO
 * @Author Tender
 * @Time 2021/7/11 9:30
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class JsonUtils {
    /**
     *
     */
    private static Gson parser;

    static {
        parser = new Gson();
    }

    /**
     *
     * @param o
     * @return
     */
    public static String toJson(Object o) {
        return parser.toJson(o);
    }

    /**
     *
     * @param s
     * @param cls
     * @param <T>
     * @return
     */
    public static <T> T fromJson(String s, Class<T> cls) {
        return parser.fromJson(s, cls);
    }
}