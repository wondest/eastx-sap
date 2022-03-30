package com.eastx.sap.rule.model;

import java.util.Collection;

/**
 * @ClassName ExpressionDeque
 * @Description: TODO
 * @Author Tender
 * @Time 2022/3/27 17:51
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public interface ExpressionQueue {
    /**
     * Get the first element in head, null if the queue is empty
     *
     * @return
     */
    Object poll();

    /**
     * Returns {@code true} if this deque contains no elements.
     *
     * @return {@code true} if this deque contains no elements
     */
    boolean isEmpty();

    /**
     *
     * @param e
     * @return {@code true} if this deque offers a element
     */
    boolean offer(Object e);

    /**
     * Fill the deque
     * @param c
     * @return {@code true} if this deque offers all elements
     */
    boolean offer(Collection c);

    /**
     * Reset the deque
     */
    default void reset() {
        throw new UnsupportedOperationException("reset");
    }
}
