package com.eastx.sap.batch.mybatisJob;

/**
 * @ClassName StockVO
 * @Description: TODO
 * @Author Tender
 * @Time 2022/1/27 20:59
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class StockVO {
    String id;

    String line;

    public String getLine() {
        return line;
    }

    public void setLine(String line) {
        this.line = line;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }
}
