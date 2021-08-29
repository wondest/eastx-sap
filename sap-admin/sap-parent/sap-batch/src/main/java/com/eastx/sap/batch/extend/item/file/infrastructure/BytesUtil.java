package com.eastx.sap.batch.extend.item.file.infrastructure;

/**
 * @ClassName BytesUtil
 * @Description: 字节数组内容的转换，使用大端模式，高位在数组首位
 * @Author Tender
 * @Time 2021/8/2 22:06
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class BytesUtil {
    /**
     * int: 4字节
     *
     * @param number
     * @return
     */
    public static byte[] fromInt(int number) {
        int len = 4;
        byte[] bytes = new byte[len];

        //0-len个字节,需要进行位移操作
        for(int i=0; i < len; i++) {
            bytes[i] = (byte)((number >> (i*4)) & 0xFF);
        }

        return bytes;
    }

    /**
     * int: 4字节
     *
     * @param bytes
     * @return
     */
    public static int asInt(byte[] bytes) {
        return asInteger(4, bytes, 0, bytes.length, false);
    }

    /**
     *
     * @param bytes
     * @param offset
     * @param length
     * @param bigEndian 大端模式 or 小端模式
     * @return
     */
    public static int asInt(byte[] bytes, int offset, int length, boolean bigEndian) {
        return asInteger(4, bytes, offset, length, bigEndian);
    }

    /**
     *
     * @param space     数字类型所占的字节数
     * @param bytes     字节数据
     * @param offset    数组偏移
     * @param length    所占长度
     * @param bigEndian
     * @return
     */
    public static int asInteger(int space, byte[] bytes, int offset, int length, boolean bigEndian) {
        int len = Math.min(length, space);

        //小端模式 低字节 位于 数组低位
        int start = 0;
        int step = 1;

        //大端模式 低字节 位于 数组高位
        if(bigEndian) {
            start = len - 1;
            step = -1;
        }

        //低位字节
        int val = (bytes[offset] & 0xFF);

        //1-len个字节,需要逐个字节0xFF得到4个int,然后|操作,得到最终结果
        for(int i=0; i < len; i++) {
            //低位|高位
            val = val|((bytes[i+offset] & 0xFF) << (i*8));
        }

        return val;
    }
}
