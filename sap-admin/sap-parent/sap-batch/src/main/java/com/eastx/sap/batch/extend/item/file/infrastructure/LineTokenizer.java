package com.eastx.sap.batch.extend.item.file.infrastructure;

import org.springframework.lang.Nullable;

/**
 * @ClassName LineTokenizer
 * @Description: tokenize a line and separate into fields
 * @Author Tender
 * @Time 2021/8/1 17:46
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface LineTokenizer {
    /**
     *
     * @param line
     * @return
     */
    FieldSet tokenize(@Nullable Separator line);
}
