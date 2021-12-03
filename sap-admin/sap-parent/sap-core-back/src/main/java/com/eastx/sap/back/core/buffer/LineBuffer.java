package com.eastx.sap.back.core.buffer;

import java.util.Arrays;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * @ClassName LineBuffer
 * @Description:
 * @Author Tender
 * @Time 2021/5/23 15:43
 * @Version 1.0
 * @Since 1.8
 **/
public class LineBuffer extends ArrayBuffer implements LineSingle {
    private int barLength;
    private int barIndex;

    private static final int RESET_BAR_INDEX = -1;
    private static final int RESET_BAR_LENGTH = 0;

    private static final BoxDouble EMPTY_VALUE = BoxDouble.NaN;

    private String name;

    LineBuffer(String name) {
        this.name = name;
        reset();
    }

    public LineBuffer() {
        this("buffer");
    }

    @Override
    public void reset() {
        // empty the data.
        clear();

        // reset the indices
        home();
    }

    @Override
    public void home() {
        // reset the indices
        this.barIndex = RESET_BAR_INDEX;
        this.barLength = RESET_BAR_LENGTH;
    }

    @Override
    public void forward() {
        forward(EMPTY_VALUE, 1);
    }

    @Override
    public void forward(int size) {
        IntStream.range(0, size).forEach(i->forward());
    }

    private void forward(BoxDouble value, int size) {
        this.barIndex += size;
        this.barLength += size;

        if(1 >= size) {
            IntStream.range(0, size).forEach(i -> append(value));
        } else {
            BoxDouble[] values = new BoxDouble[size];
            IntStream.range(0, size).forEach(i -> values[i]=value);

            append(Arrays.asList(values));
        }
    }

    @Override
    public void backwards() {
        backwards(1);
    }

    @Override
    public void backwards(int size) {
        this.barIndex -= size;
        this.barLength -= size;
    }

    @Override
    public void advance() {
        advance(1);
    }

    @Override
    public void advance(int size) {
        this.barIndex += size;
        this.barLength += size;
    }

    @Override
    public void rewind() {
        rewind(1);
    }

    @Override
    public void rewind(int size) {
        this.barIndex -= size;
        this.barLength -= size;
    }

    private int base(int offset) {
        return this.barIndex + offset;
    }

    @Override
    public void setBar(int offset, BoxDouble item) {
        set(base(offset), item);
    }

    @Override
    public void setBar(BoxDouble item) {
        setBar(0, item);
    }

    @Override
    public BoxDouble getBar(int offset) {
        return get(base(offset));
    }

    @Override
    public BoxDouble getBar() {
        return get(base(0));
    }

    @Override
    public Stream<BoxDouble> between(int startInclusive, int endExclusive) {
        return slice(startInclusive, endExclusive);
    }

    @Override
    public int barLen() {
        return barLength;
    }

    @Override
    public int bufLen() {
        return size();
    }

    @Override
    public int barIndex() {
        return barIndex;
    }

    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();

        sb.append(name).append(":").append("[BoxDouble").append("(").append(size()).append(")");
        IntStream.range(0, size()).forEach(i->sb.append(",(").append(i).append(")").append(get(i)));
        sb.append("]");

        return sb.toString();
    }
}
