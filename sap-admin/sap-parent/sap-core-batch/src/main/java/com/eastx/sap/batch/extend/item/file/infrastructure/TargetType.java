package com.eastx.sap.batch.extend.item.file.infrastructure;

import org.springframework.beans.BeanUtils;
import org.springframework.util.Assert;

import javax.el.MethodNotFoundException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.function.Function;

/**
 * @ClassName Range
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/1 20:56
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class TargetType<T> {
    /**
     * 目标类型
     */
    private final Class<T> targetType;

    /**
     *
     */
    private Range[] ranges;

    /**
     *
     */
    private String[] names;

    /**
     *
     */
    private int chunkSize = Integer.MIN_VALUE;

    /**
     *
     * @param type
     */
    public TargetType(Class<T> type) {
        Assert.notNull(type, "Input type should not be null");
        this.targetType = type;
        initializeBefore();
    }

    /**
     * Using
     */
    private void initializeBefore() {
        int length = targetType.getDeclaredFields().length;

        this.ranges = new Range[length];
        this.names = new String[length];

        int next = 0;
        int i = 0;

        //由于ranges必须保持声明时候的顺序,此处尽量不使用stream
        for(Field f : this.targetType.getDeclaredFields()) {
            //add names
            this.names[i] = f.getName();

            //add ranges
            Class type = f.getType();
            if (short.class.equals(type) || Short.class.equals(type)) {
                this.ranges[i] = Range.valueOf(next, 2);
                next += 2;
            } else if (int.class.equals(type) || Integer.class.equals(type)) {
                this.ranges[i] = Range.valueOf(next, 4);
                next += 4;
            } else if (long.class.equals(type) || Long.class.equals(type)) {
                this.ranges[i] = Range.valueOf(next, 8);
                next += 8;
            } else {
                //throw new TypeNotSupportedException(type.getTypeName());
            }

            //increase
            i++;
        }

        // set chunkSize
        this.chunkSize = next;
    }

    /**
     *
     * @return
     */
    public Range[] getRanges() {
        return ranges;
    }

    /**
     *
     * @return
     */
    public String[] getNames() {
        return names;
    }

    /**
     *
     * @return
     */
    public int getChunkSize() {
        return chunkSize;
    }

    /**
     *
     * @param properties
     * @return
     * @throws ReflectiveOperationException
     */
    public T getAndSetTarget(Function<String, Separator> properties) throws ReflectiveOperationException  {
        T target = targetType.getDeclaredConstructor().newInstance();

        //BeanUtils.getPropertyDescriptors是按照属性名称排序的
        //getDeclaredFields是按照定义的顺序排序的
        for (Field field : targetType.getDeclaredFields()) {
            PropertyDescriptor targetPd = BeanUtils.getPropertyDescriptor(targetType, field.getName());
            Separator value = properties.apply(targetPd.getName());

            Method writeMethod = targetPd.getWriteMethod();
            Class type = targetPd.getPropertyType();

            if (writeMethod != null) {
                if (!Modifier.isPublic(writeMethod.getDeclaringClass().getModifiers())) {
                    writeMethod.setAccessible(true);
                }

                if (short.class.equals(type) || Short.class.equals(type)) {
                    writeMethod.invoke(target, value.toInt());
                } else if (int.class.equals(type) || Integer.class.equals(type)) {
                    writeMethod.invoke(target, value.toInt());
                } else if (long.class.equals(type) || Long.class.equals(type)) {
                    writeMethod.invoke(target, value.toInt());
                } else {
                    throw new TypeNotSupportedException(type.getTypeName());
                }
            } else {
                throw new MethodNotFoundException(String.format("The field %s has no write method", new Object[] {targetPd.getName()}));
            }
        }

        return target;
    }
}
