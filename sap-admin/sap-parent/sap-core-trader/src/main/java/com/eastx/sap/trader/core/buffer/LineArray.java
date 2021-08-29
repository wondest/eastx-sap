package com.eastx.sap.trader.core.buffer;

import java.util.Collection;
import java.util.stream.Stream;

/**
 * 基本数组的接口
 *
 * @author Tender
 */
public interface LineArray<E> {
    /**
     * 返回Vector的元素个数
     *
     * @return
     */
    public int size();

    /**
     * 设置Vector的第index个元素的值
     *
     * @param index
     * @param element
     * @return
     */
    public E set(int index, E element);

    /**
     * 设置Vector的第index个元素的值(double)
     * @param index
     * @param element
     * @return
     */
    public E set(int index, double element);

    /**
     * 返回Vector的第index个元素的值
     *
     * @param index
     * @return
     */
    public E get(int index);

    /**
     * 清空Vector
     */
    public void clear();

    /**
     * 在Vector追加一个元素
     * @param element
     * @return
     */
    public boolean append(E element);

    /**
     * 在Vector批量追加元素
     * @param c
     * @return
     */
    public boolean append(Collection<E> c);

    /**
     * 获取Vector指定区间的子序列引用或者副本
     * @param startInclusive
     * @param endExclusive
     * @return
     */
    public Stream<E> slice(int startInclusive, int endExclusive);
}
