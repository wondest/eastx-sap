package com.eastx.sap.data.type;

import javax.persistence.AttributeConverter;
import java.util.Arrays;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * @ClassName AbstractConverter
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/7 23:13
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public abstract class AbstractConverter<T> implements AttributeConverter<T, Integer> {

    Function<T, Integer> converter;

    AbstractConverter(Function<T, Integer> converter) {
        this.converter = converter;
    }
    @Override
    public Integer convertToDatabaseColumn(T attribute) {
        return converter.apply(attribute);
    }

    @Override
    public T convertToEntityAttribute(Integer dbData) {
        return Arrays.stream(allValues())
                .filter(v->converter.apply(v).equals(dbData))
                .findFirst().orElse(null);
    }

    abstract T[] allValues();
}
