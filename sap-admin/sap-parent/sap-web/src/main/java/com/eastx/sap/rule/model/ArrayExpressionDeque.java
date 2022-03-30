package com.eastx.sap.rule.model;

import java.util.ArrayList;
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
public class ArrayExpressionDeque implements ExpressionQueue {
    /**
     * The stack for bool expression,the object is [ operand->OperandEnum, operator->AbstractChainedEvaluator ]
     */
    private ArrayList<Object> elements = new ArrayList<>();

    /**
     * The index of the element at the head of the deque (which is the
     * element that would be removed by remove() or pop()); or an
     * arbitrary number 0 <= head < elements.length equal to tail if
     * the deque is empty.
     */
    transient int head;

    /**
     * The index at which the next element would be added to the tail
     * of the deque (via addLast(E), add(E), or push(E));
     * elements[tail] is always null.
     */
    transient int tail;

    public ArrayExpressionDeque() {
        this.head = 0;
        this.tail = 0;
    }

    @Override
    public Object poll() {
        if(isEmpty()) {
            return null;
        } else {
            return elements.get(head++);
        }
    }

    @Override
    public boolean isEmpty() {
        return head == tail;
    }

    @Override
    public boolean offer(Object e) {
        if(null == e) {
            return false;
        }

        if(elements.add(e)) {
            tail++;
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean offer(Collection c) {
        if(null == c) {
            return false;
        }

        if(elements.addAll(c)) {
            tail += c.size();
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void reset() {
        head = 0;
        tail = elements.size();
    }
}
