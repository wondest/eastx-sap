package com.eastx.sap.rule.core;

/**
 *
 * @param <T>
 */
public interface Rule<T> {
    void fire(T fact);
}
