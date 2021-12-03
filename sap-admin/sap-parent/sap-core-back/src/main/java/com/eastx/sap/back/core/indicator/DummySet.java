package com.eastx.sap.back.core.indicator;

import com.eastx.sap.back.core.buffer.BoxDouble;
import com.eastx.sap.back.core.buffer.LineSingle;
import com.eastx.sap.back.core.indicator.base.AbstractIndicators;

import java.util.Arrays;

/**
 * @ClassName DummySet
 * @Description: 无效指标
 * @Author Tender
 * @Time 2021/5/23 16:38
 * @Version 1.0
 * @Since 1.8
 **/
public class DummySet extends AbstractIndicators.Eval implements Indicator {
    /**
     * 输入的源数据
     */
    private final LineSingle data0;

    public DummySet(Indicator ind0, Indicator... indicators) {
        super("DummySet", ind0.period(), ind0);

        data0 = addIndicator(ind0);

        Arrays.stream(indicators).forEach(this::addIndicator);
    }

    @Override
    protected void doEvalOnce(int i) {
        set(i, BoxDouble.ZERO);
    }

    @Override
    protected void doEvalNext() {
        doEvalOnce(barIndex());
    }

    @Override
    protected void nextFirst() {
        doEvalNext();
    }

    @Override
    protected void nextRemaining() {
        doEvalNext();
    }
}
