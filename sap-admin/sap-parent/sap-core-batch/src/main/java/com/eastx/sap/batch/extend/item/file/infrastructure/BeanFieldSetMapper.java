package com.eastx.sap.batch.extend.item.file.infrastructure;


import java.util.function.Function;

/**
 * @ClassName BeanFieldSetMapper
 * @Description: 使用反射方法将FileSet内容填充到具体的对象实例中，并返回
 * @Author Tender
 * @Time 2021/8/1 20:32
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class BeanFieldSetMapper<T> implements FieldSetMapper<T> {
    /**
     * The return class type
     */
    private TargetType targetType;

    /**
     *
     * @param type
     */
    public BeanFieldSetMapper(TargetType type) {
        this.targetType = type;
    }

    /**
     *
     * @param fs
     * @return
     * @throws IllegalAccessException
     */
    @Override
    public T mapFieldSet(FieldSet fs) throws ReflectiveOperationException {
        return (T) targetType.getAndSetTarget((Function<String, Separator>) k->fs.get(k));
    }
}
