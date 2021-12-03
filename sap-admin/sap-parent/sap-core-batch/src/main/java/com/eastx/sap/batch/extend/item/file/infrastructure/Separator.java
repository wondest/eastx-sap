package com.eastx.sap.batch.extend.item.file.infrastructure;

/**
 * @ClassName Separator
 * @Description: 词项切割
 * @Author Tender
 * @Time 2021/8/1 17:22
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface Separator {
    /**
     *
     * @param startInclusive
     * @param endExclusive
     * @return
     */
    Separator slice(int startInclusive, int endExclusive);

    /**
     * Convert to a int value
     * @return
     */
    int toInt();
}
