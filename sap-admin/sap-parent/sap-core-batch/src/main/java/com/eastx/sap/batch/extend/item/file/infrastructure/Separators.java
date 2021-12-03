package com.eastx.sap.batch.extend.item.file.infrastructure;

import java.util.Objects;

/**
 * @ClassName Separator
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/1 17:22
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public final class Separators {
    /**
     * Create a Separator.OfByte covering a range of elements of a given array
     * @param array
     * @param startInclusive The least index (inclusive) to cover
     * @param endExclusive The greatest index (exclusive) to cover
     * @return
     */
    public static Separator separator(byte[] array, int startInclusive, int endExclusive) {
        checkFromToBounds(Objects.requireNonNull(array).length, startInclusive, endExclusive);
        return new Separators.ByteArraySeparator(array, startInclusive, endExclusive);
    }

    /**
     * Create a Separator.OfByte covering a range of elements of a given array
     * @param array
     * @return
     */
    public static Separator separator(byte[] array) {
        return new Separators.ByteArraySeparator(Objects.requireNonNull(array));
    }

    /**
     * For Bytes
     */
    static final class ByteArraySeparator implements Separator {
        private final byte[] array;
        private final int start;    // The least index (inclusive)
        private final int end;     // The used length (exclusive)
        private final int length;

        /**
         *
         * @param array
         */
        ByteArraySeparator(byte[] array) {
            this(array, 0, array.length);
        }

        /**
         *
         * @param array
         * @param startInclusive Inclusive
         * @param endExclusive Exclusive
         */
        ByteArraySeparator(byte[] array, int startInclusive, int endExclusive) {
            this.array = array;
            this.start = startInclusive;
            this.end = endExclusive;
            this.length = endExclusive - startInclusive;
        }

        @Override
        public Separator slice(int fromInclusive, int toExclusive) {
            return Separators.separator(this.array, start + fromInclusive, start + toExclusive);
        }

        @Override
        public int toInt() {
            return BytesUtil.asInt(array, start, length, false);
        }
    }

    /**
     *
     * @param arrayLength
     * @param startIndex
     * @param endIndex
     */
    private static void checkFromToBounds(int arrayLength, int startIndex, int endIndex) {
        if (startIndex > endIndex) {
            throw new ArrayIndexOutOfBoundsException(
                    "start(" + startIndex + ") > end(" + endIndex + ")");
        }
        if (startIndex < 0) {
            throw new ArrayIndexOutOfBoundsException(startIndex);
        }
        if (endIndex > arrayLength) {
            throw new ArrayIndexOutOfBoundsException(endIndex);
        }
    }
}
