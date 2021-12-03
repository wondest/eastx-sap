package com.eastx.sap.batch.extend.item.file.infrastructure;

/**
 * @ClassName TypeNotSupportException
 * @Description: TODO
 * @Author Tender
 * @Time 2021/8/2 22:44
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
public class TypeNotSupportedException extends RuntimeException {
    @java.io.Serial
    private static final long serialVersionUID = -5101214195716534496L;

    /**
     * The type name.
     */
    private String typeName;

    /**
     * Constructs a {@code TypeNotPresentException} for the named type
     * with the specified cause.
     *
     * @param typeName the fully qualified name of the unavailable type
     * @param cause the exception that was thrown when the system attempted to
     *    load the named type, or {@code null} if unavailable or inapplicable
     */
    public TypeNotSupportedException(String typeName, Throwable cause) {
        super("Type " + typeName + " is not supported", cause);
        this.typeName = typeName;
    }

    /**
     * Constructs a {@code TypeNotPresentException} for the named type
     * with the specified cause.
     *
     * @param typeName the fully qualified name of the unavailable type
     */
    public TypeNotSupportedException(String typeName) {
        super("Type " + typeName + " is not supported");
        this.typeName = typeName;
    }

    /**
     * Returns the fully qualified name of the unavailable type.
     *
     * @return the fully qualified name of the unavailable type
     */
    public String typeName() { return typeName;}
}
