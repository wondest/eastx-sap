package com.eastx.sap.batch.extend.item.file.infrastructure;

/**
 * @ClassName ObjectFieldSetFactory
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/1 20:08
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class DefaultFieldSetFactory implements FieldSetFactory {
    /**
     * 这里不再重复检查 names和values的对应关系,在token中已经有检查了
     *
     * @param names
     * @param fields
     * @return
     */
    @Override
    public FieldSet create(String[] names, Separator[] fields) {
        return new DefaultFieldSet(names, fields);
    }
}