package com.eastx.sap.batch.extend.item.file.infrastructure;

/**
 * 将一行原始数据映射成需要的对象
 *
 * @param <T>
 */
public interface LineMapper<T> {
    /**
     *
     * @param line
     * @param lineNum
     * @return
     * @throws Exception
     */
    T mapLine(Separator line, int lineNum) throws ReflectiveOperationException;
}
