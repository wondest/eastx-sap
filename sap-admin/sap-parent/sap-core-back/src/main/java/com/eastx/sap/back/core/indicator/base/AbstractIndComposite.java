package com.eastx.sap.back.core.indicator.base;

import com.eastx.sap.back.core.buffer.BoxDouble;
import com.eastx.sap.back.core.buffer.BufferSupport;
import com.eastx.sap.back.core.buffer.LineSingle;
import com.eastx.sap.back.core.indicator.Indicator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Stream;

/**
 * @ClassName IndicatorSeries
 * @Description: 组合模式
 * @Author Tender
 * @Time 2021/5/23 15:58
 * @Version 1.0
 * @Since 1.8
 **/
public abstract class AbstractIndComposite implements Indicator {
    /**
     * The children indicators.
     */
    private List<Indicator> indicators;

    /**
     * The line of the indicator.
     */
    private LineSingle line;

    /**
     * The period of the indicator.
     */
    private int period;

    /**
     * The clock of the indicator.
     */
    private final LineSingle clock;

    /**
     * The line is a binding by a child indicator or self's evaluation.
     */
    private boolean isBinding;

    /**
     * The identity of the indicator, for logging.
     */
    private String name;

    AbstractIndComposite(String name, int period, final LineSingle clock, boolean isBinding) {
        this.name = name;
        this.period = period;
        this.clock = clock;
        this.isBinding = isBinding;
        this.indicators = new ArrayList<Indicator>();

        //如果使用Binding,那么结果由children indicator绑定结果,否则结果由本指标进行计算得到
        if(isBinding) {
            this.line = null;
        } else {
            this.line = BufferSupport.newLine();
        }
    }

    protected Indicator addIndicator(Indicator indicator) {
        this.indicators.add(indicator);
        return indicator;
    }

    /**
     * 递归方式：广度遍历
     */
    @Override
    public void evalOnce() {
        //self binding
        bindLine();

        //self forward
        onceForward();

        //children's evaluation
        indicators.stream().forEach(i->i.evalOnce());

        //self evaluation
        onceBefore(0, period-1);
        onceFirst(period-1, period);
        onceRemaining(period, clock.size());
    }

    @Override
    public void evalNext() {
        if (clock.barLen() == 1) {
            bindLine();
        }

        //self forward
        nextForward();

        //children's evaluation
        indicators.stream().forEach(i->i.evalNext());

        //self evaluation
        if(clock.barLen() > period) {
            nextRemaining();
        } else if (clock.barLen() == period) {
            nextFirst();
        } else {
            nextBefore();
        }
    }

    /**
     * 启动前处理
     * @param startInclusive
     * @param endExclusive
     */
    protected abstract void onceBefore(int startInclusive, int endExclusive);

    /**
     * 启动处理第一个元素
     * @param startInclusive
     * @param endExclusive
     */
    protected abstract void onceFirst(int startInclusive, int endExclusive);

    /**
     * 启动处理后续元素
     * @param startInclusive
     * @param endExclusive
     */
    protected abstract void onceRemaining(int startInclusive, int endExclusive);

    /**
     * 启动前处理
     */
    protected abstract void nextBefore();

    /**
     * 启动处理第一个元素
     */
    protected abstract void nextFirst();

    /**
     * 启动处理后续元素
     */
    protected abstract void nextRemaining();

    /**
     * 推进一个Bar
     */
    private void nextForward() {
        //确保比时钟走的慢
        if(!isBinding && clock.barLen() > line.barLen()) {
            line.forward();
        }
    }

    /**
     * 根据时钟一次性推进所有Bar
     */
    private void onceForward() {
        if(!isBinding) {
            line.forward(clock.size());
            line.home();
        }
    }

    /**
     * 绑定当前的Line
     */
    private void bindLine() {
        if(isBinding) {
            line = proxyLine();
        }
    }

    /**
     *
     * @return
     */
    abstract protected LineSingle proxyLine();

    @Override
    public LineSingle getLine() {
        return this.line;
    }

    @Override
    public int period() {
        return period;
    }

    @Override
    public int size() {
        return line.size();
    }

    @Override
    public BoxDouble set(int index, BoxDouble element) {
        return line.set(index, element);
    }

    @Override
    public BoxDouble set(int index, double element) {
        return line.set(index, element);
    }

    @Override
    public BoxDouble get(int index) {
        return line.get(index);
    }

    @Override
    public void clear() {
        line.clear();
    }

    @Override
    public boolean append(BoxDouble element) {
        return line.append(element);
    }

    @Override
    public boolean append(Collection<BoxDouble> c) {
        return line.append(c);
    }

    @Override
    public Stream<BoxDouble> slice(int startInclusive, int endExclusive) {
        return line.slice(startInclusive, endExclusive);
    }

    @Override
    public Stream<BoxDouble> between(int startInclusive, int endExclusive) {
        return line.between(startInclusive, endExclusive);
    }

    @Override
    public void reset() {
        line.reset();
    }

    @Override
    public void home() {
        line.home();
    }

    @Override
    public void forward() {
        line.forward();
    }

    @Override
    public void forward(int size) {
        line.forward(size);
    }

    @Override
    public void backwards() {
        line.backwards();
    }

    @Override
    public void backwards(int size) {
        line.backwards(size);
    }

    @Override
    public void setBar(int offset, BoxDouble item) {
        line.setBar(offset, item);
    }

    @Override
    public void setBar(BoxDouble item) {
        line.setBar(item);
    }

    @Override
    public void rewind() {
        line.rewind();
    }

    @Override
    public void rewind(int size) {
        line.rewind(size);
    }

    @Override
    public void advance() {
        line.advance();
    }

    @Override
    public void advance(int size) {
        line.advance(size);
    }

    @Override
    public BoxDouble getBar() {
        return line.getBar();
    }

    @Override
    public BoxDouble getBar(int offset) {
        return line.getBar(offset);
    }

    @Override
    public int barLen() {
        return line.barLen();
    }

    @Override
    public int bufLen() {
        return line.bufLen();
    }

    @Override
    public int barIndex() {
        return line.barIndex();
    }

    @Override
    public String toString() {
        StringBuffer sb = new StringBuffer();
        return sb.append(name).append("_").append(line).toString();
    }
}