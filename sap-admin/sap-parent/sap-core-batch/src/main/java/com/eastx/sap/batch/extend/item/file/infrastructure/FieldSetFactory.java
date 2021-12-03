package com.eastx.sap.batch.extend.item.file.infrastructure;

/**
 *
 */
public interface FieldSetFactory {
    /**
     * @param names
     * @param fields
     * @return
     */
    FieldSet create(String[] names, Separator[] fields);
}
