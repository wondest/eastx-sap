package com.eastx.sap.batch.extend.item.file.infrastructure;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;

/**
 *
 */
public class DefaultFieldSet implements FieldSet {
    /**
     *
     */
    private Map<String, Separator> properties;

    /**
     *
     * @param keys
     * @param values
     */
    DefaultFieldSet(String[] keys, Separator[] values) {
        this.properties = new HashMap<>();
        IntStream.range(0, keys.length).forEach(i->properties.put(keys[i], values[i]));
    }

    /**
     *
     * @param key
     * @return
     */
    public Separator get(String key) {
        return properties.get(key);
    }
}
