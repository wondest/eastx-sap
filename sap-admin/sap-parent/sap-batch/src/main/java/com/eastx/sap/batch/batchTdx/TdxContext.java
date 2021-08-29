package com.eastx.sap.batch.batchTdx;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

/**
 * @ClassName TdxContext
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/6 0:02
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Component
public class TdxContext {
    @Value("${user.path.tdx.home}")
    private String source;

    @Value("${user.path.output}")
    private String output;

    public String checkCode(final String code) {
        Assert.notNull(code, "Input code should not be null");
        Assert.isTrue(code.length() == 8, "Input code should be 8 char");
        String exchange = code.substring(0, 2).toUpperCase();
        Assert.isTrue("SZ".equals(exchange) || "SH".equals(exchange), "Input code should like sz000001 or sh000001");
        return code.toLowerCase();
    }

    private String getDayFile(final String code) {
        String lowerCode = code.toLowerCase();
        String exchange = lowerCode.substring(0,2);
        return new StringBuilder(source)
                .append("/")
                .append("vipdoc/")
                .append(exchange).append("/")
                .append("lday/")
                .append(lowerCode)
                .append(".day")
                .toString();
    }

    private String getOutFile(final String code) {
        String lowerCode = code.toLowerCase();
        return new StringBuilder(output)
                .append("/")
                .append(lowerCode)
                .append(".csv")
                .toString();
    }


    public String[] getPairFile(final String code) {
        checkCode(code);
        String pairFile[] = new String[2];

        pairFile[0] = getDayFile(code);
        pairFile[1] = getOutFile(code);

        return pairFile;
    }

}
