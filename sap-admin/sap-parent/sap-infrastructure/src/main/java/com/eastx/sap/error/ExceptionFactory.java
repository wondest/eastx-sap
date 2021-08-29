package com.eastx.sap.error;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.MessageSource;
import org.springframework.context.NoSuchMessageException;

import java.util.Locale;

/**
 * @ClassName ExtendRuntimeException
 * @Description: 传入
 * @Author Tender
 * @Time 2021/7/13 22:16
 * @Version 1.0
 * @Since 1.8
 * @Copyright ©2021-2021 Tender Xie, All Rights Reserved.
 **/
@Slf4j
public class ExceptionFactory {
    /**
     * 国际化
     */
    private MessageSource i18n;

    public ExceptionFactory(MessageSource i18n) {
        this.i18n = i18n;
    }

    /**
     *
     * @param code
     * @return
     */
    public ExtendRuntimeException newException(ErrorEnum code) {
        String message;
        try {
            message = i18n.getMessage(code.getI18n(), null, Locale.getDefault());
        } catch (NoSuchMessageException e) {
            log.warn(i18n.getMessage("message.missing", new Object[]{code}, Locale.getDefault()), e);
            message = code.getCode();
        }

        return new ExtendRuntimeException(code.getCode(), message);
    }

    /**
     *
     * @param code
     * @param args
     * @return
     */
    public ExtendRuntimeException newException(ErrorEnum code, Object...args) {
        String message;
        try {
            message = i18n.getMessage(code.getI18n(), args, Locale.getDefault());
        } catch (NoSuchMessageException e) {
            log.warn(i18n.getMessage("message.missing", new Object[]{code}, Locale.getDefault()), e);
            message = code.getCode();
        }

        return new ExtendRuntimeException(code.getCode(), message);
    }

    /**
     *
     * @param code
     * @param args
     * @return
     */
    public ExtendRuntimeException newException(ErrorEnum code, Throwable cause, Object...args) {
        String message;
        try {
            message = i18n.getMessage(code.getI18n(), args, Locale.getDefault());
        } catch (NoSuchMessageException e) {
            log.warn(i18n.getMessage("message.missing", new Object[]{code}, Locale.getDefault()), e);
            message = code.getCode();
        }

        return new ExtendRuntimeException(code.getCode(), message, cause);
    }
}
