package com.eastx.sap.trader.core.buffer;

import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * @ClassName AbstractLines
 * @Description: 多线的抽象父类
 * @Author Tender
 * @Time 2021/5/23 15:43
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public abstract class AbstractLines implements LineMultiple {
    /**
     * The array of the values.
     */
    private LineBuffer[] lines;

    /**
     * The size of the line array.
     */
    private int lineSize;

    /**
     * The master line within the line array.
     */
    private LineBuffer master;

    public AbstractLines(int lineSize) {
        this.lineSize = lineSize;
        this.lines = new LineBuffer[lineSize];
        IntStream.range(0, lineSize).forEach(i -> this.lines[i] = new LineBuffer());
        this.master = lines[0];
    }

    public AbstractLines(String[] lineAlias) {
        checkNotNull(lineAlias, "lineAlias must not be null");
        checkArgument(lineAlias.length > 0, "lineAlias must not be empty");

        this.lineSize = lineAlias.length;
        this.lines = new LineBuffer[lineSize];
        IntStream.range(0, lineSize).forEach(i -> this.lines[i] = new LineBuffer(lineAlias[i]));
        this.master = lines[0];
    }

    @Override
    public void reset() {
        IntStream.range(0, lineSize).forEach(i->lines[i].reset());
    }

    @Override
    public void home() {
        IntStream.range(0, lineSize).forEach(i->lines[i].home());
    }

    @Override
    public void forward() {
        IntStream.range(0, lineSize).forEach(i->lines[i].forward());
    }

    @Override
    public void forward(int size) {
        IntStream.range(0, lineSize).forEach(i->lines[i].forward(size));
    }

    @Override
    public void backwards() {
        IntStream.range(0, lineSize).forEach(i->lines[i].backwards());
    }

    @Override
    public void backwards(int size) {
        IntStream.range(0, lineSize).forEach(i->lines[i].backwards(size));
    }

    @Override
    public void setBar(int offset, BoxDouble item) {
        IntStream.range(0, lineSize).forEach(i->lines[i].setBar(offset, item));
    }

    @Override
    public void setBar(BoxDouble item) {
        IntStream.range(0, lineSize).forEach(i->lines[i].setBar(item));
    }

    @Override
    public BoxDouble getBar(int offset) {
        return lines[0].getBar(offset);
    }

    @Override
    public Stream<BoxDouble> between(int startInclusive, int endExclusive) {
        return master.slice(startInclusive, endExclusive);
    }

    @Override
    public LineBuffer getLine(int order) {
        return lines[order];
    }

    @Override
    public void advance() {
        IntStream.range(0, lineSize).forEach(i->lines[i].advance());
    }

    @Override
    public void advance(int size) {
        IntStream.range(0, lineSize).forEach(i->lines[i].advance(size));
    }

    @Override
    public void rewind() {
        IntStream.range(0, lineSize).forEach(i->lines[i].rewind());
    }

    @Override
    public void rewind(int size) {
        IntStream.range(0, lineSize).forEach(i->lines[i].rewind(size));
    }

    @Override
    public BoxDouble getBar() {
        return master.getBar();
    }

    @Override
    public int barLen() {
        return master.barLen();
    }

    @Override
    public int bufLen() {
        return master.bufLen();
    }

    @Override
    public int barIndex() {
        return master.barIndex();
    }
}
