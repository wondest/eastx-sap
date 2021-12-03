package com.eastx.sap.batch.extend.item.file.infrastructure;

import static com.google.common.base.Preconditions.checkArgument;

/**
 * @ClassName Range
 * @Description:
 * @Author Tender
 * @Time 2021/8/1 20:56
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class Range {
    /**
     * 下边界
     */
    private final int lower;

    /**
     * 上边界
     */
    private final int upper;

    /**
     * 总长度
     */
    private final int length;

    /**
     *  [start, upper)
     *
     * @param start
     * @param upper
     * @param offset
     */
    Range(int start, int upper, int offset) {
        this.lower = start;
        this.upper = start + offset;
        this.length = offset;

        checkBounds(lower, upper);
    }

    /**
     *
     * @return
     */
    public int getUpper() {
        return this.upper;
    }

    /**
     *
     * @return
     */
    public int getLower() {
        return this.lower;
    }

    /**
     * Decide
     * @return
     */
    public boolean isInfinity() {
        return Integer.MAX_VALUE == this.upper;
    }

    /**
     * Range String
     * @return
     */
    public String toString() {
        return this.isInfinity() ? this.lower + "-" + this.upper : String.valueOf(this.lower);
    }

    /**
     * Check the bound of the range
     * @param lower
     * @param upper
     */
    private void checkBounds(int lower, int upper) {
        checkArgument(lower >= 0, "Lower value %s must be greater than or equal to zero", lower);
        checkArgument(lower <= upper, "Lower value %s should be less than or equal to upper value %s", lower, upper);
    }

    /**
     * Create an infinity range
     * @param start
     * @return
     */
    public static Range infinity(int start) {
        return new Range(start, Integer.MAX_VALUE, Integer.MAX_VALUE - start + 1);
    }

    /**
     * Create a custom range
     * @param start
     * @param offset
     * @return
     */
    public static Range valueOf(int start, int offset) {
        return new Range(start, start + offset -1, offset);
    }
}
