package com.eastx.sap.back.core.buffer;

import com.eastx.sap.util.DateUtils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.BinaryOperator;

/**
 * @ClassName BigDouble
 * @Description: 原子类型
 * @Author Tender
 * @Time 2021/6/13 14:13
 * @Version 1.0
 * @Since 1.8
 **/
public final class BoxDouble extends Number implements Comparable<BoxDouble> {
    /**
     * The value of NaN.
     */
    public static final BoxDouble NaN = new BoxDouble(true);

    /**
     * The string of NaN.
     */
    public static final String DISPLAY_STRING_NAN = "NaN";

    /**
     * The value constant 0.
     */
    public static final BoxDouble ZERO = new BoxDouble(BigDecimal.ZERO);

    /**
     * The value constant 1.
     */
    public static final BoxDouble ONE = new BoxDouble(BigDecimal.ONE);

    /**
     * The value constant -1.
     */
    private static final BoxDouble NEGATIVE_ONE = new BoxDouble(new BigDecimal(BigInteger.valueOf(-1)));

    /**
     * The value of the Double.
     *
     * @serial
     */
    private final BigDecimal value;

    /**
     *  Cache the time value.
     */
    private transient long time;

    /**
     * The flag of the time.
     *
     * @serial
     */
    private final boolean isDate;

    /**
     * The flag of the NaN.
     *
     * @serial
     */
    private final boolean isNaN;

    BoxDouble(double value) {
        this(BigDecimal.valueOf(value));
    }

    BoxDouble(Double value) {
        this(BigDecimal.valueOf(value));
    }

    BoxDouble(String value) {
        this(BigDecimal.valueOf(Double.parseDouble(value)));
    }

    BoxDouble(long value) {
        this.value = new BigDecimal(BigInteger.valueOf(value));
        this.isNaN = false;
        this.time = value;
        this.isDate = true;
    }

    BoxDouble(BigDecimal value) {
        this.value = value;
        this.isNaN = false;
        this.time = value.longValue();
        this.isDate = false;
    }

    BoxDouble(boolean isNaN) {
        this.value = BigDecimal.ZERO;
        this.isNaN = true;
        this.isDate = false;
    }

    public static BoxDouble valueOf(double d) {
        return new BoxDouble(d);
    }

    public static BoxDouble valueOf(Double d) {
        return new BoxDouble(d);
    }

    public static BoxDouble valueOf(String s) {
        return new BoxDouble(s);
    }

    public static BoxDouble valueOf(BigDecimal d) {
        return new BoxDouble(d);
    }

    public static BoxDouble dateOf(long n) {
        return new BoxDouble(n);
    }

    @Override
    public int compareTo(BoxDouble another) {
        return this.value.compareTo(another.value);
    }

    @Override
    public int intValue() {
        return value.intValue();
    }

    @Override
    public long longValue() {
        return value.longValue();
    }

    @Override
    public float floatValue() {
        return value.floatValue();
    }

    @Override
    public double doubleValue() {
        return value.doubleValue();
    }

    public BigDecimal value() {
        return value;
    }

    public boolean isNaN() {
        return this.isNaN;
    }

    public boolean isDate() {
        return this.isDate;
    }

    @Override
    public String toString() {
        return toString(this);
    }

    public String toString(BoxDouble box) {
        if(box.isNaN()) {
            return DISPLAY_STRING_NAN;
        } else if(box.isDate()) {
            return DateUtils.num2Str(box.time);
        } else {
            return box.value.toString();
        }
    }

    private static BoxDouble ensureNaN(BoxDouble a, BoxDouble b, BinaryOperator<BoxDouble> operator) {
        return (!a.isNaN() && !b.isNaN())?operator.apply(a,b):NaN;
    }

    private static BoxDouble _add_(BoxDouble a, BoxDouble b) {
        return new BoxDouble(a.value().add(b.value()));
    }

    private static BoxDouble _sub_(BoxDouble a, BoxDouble b) {
        return new BoxDouble(a.value().subtract(b.value()));
    }

    private static BoxDouble _mul_(BoxDouble a, BoxDouble b) {
        return new BoxDouble(a.value().multiply(b.value()));
    }

    private static BoxDouble _div_(BoxDouble a, BoxDouble b) {
        return new BoxDouble(a.value().divide(b.value(), BigDecimal.ROUND_HALF_DOWN));
    }

    public BoxDouble add(BoxDouble another) {
        return ensureNaN(this, another, BoxDouble::_add_);
    }

    public BoxDouble sub(BoxDouble another) {
        return ensureNaN(this, another, BoxDouble::_sub_);
    }

    public BoxDouble mul(BoxDouble another) {
        return ensureNaN(this, another, BoxDouble::_mul_);
    }

    public BoxDouble div(BoxDouble another) {
        return ensureNaN(this, another, BoxDouble::_div_);
    }
}