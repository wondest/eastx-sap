package com.eastx.sap.back.core.buffer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Stream;

/**
 * @ClassName BufferArray
 * @Description: 基本的向量操作
 * @Author Tender
 * @Time 2021/5/23 15:41
 * @Version 1.0
 * @Since 1.8
 **/
public class ArrayBuffer implements LineArray<BoxDouble> {
    /**
     *
     */
    private ArrayList<BoxDouble> array = new ArrayList<BoxDouble>();

    @Override
    public int size() {
        return array.size();
    }

    @Override
    public BoxDouble set(int index, BoxDouble element) {
        return array.set(index, element);
    }

    @Override
    public BoxDouble set(int index, double element) {
        return set(index, BoxDouble.valueOf(element));
    }

    @Override
    public BoxDouble get(int index) {
        return array.get(index);
    }

    @Override
    public void clear() {
        array.clear();
    }

    @Override
    public boolean append(BoxDouble element) {
        return array.add(element);
    }

    @Override
    public boolean append(Collection<BoxDouble> c) {
        return array.addAll(c);
    }

    @Override
    public Stream<BoxDouble> slice(int startInclusive, int endExclusive) {
        return array.subList(startInclusive, endExclusive).stream();
    }
}