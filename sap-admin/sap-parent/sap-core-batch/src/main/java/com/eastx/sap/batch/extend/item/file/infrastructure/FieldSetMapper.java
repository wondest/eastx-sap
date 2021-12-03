package com.eastx.sap.batch.extend.item.file.infrastructure;

/**
 *
 */
public interface FieldSetMapper<T> {
    /**
     *
     * @param fs
     * @return
     */
    T mapFieldSet(FieldSet fs) throws ReflectiveOperationException;
}
