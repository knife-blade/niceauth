package com.suchtool.niceauth.annotation;

import java.lang.annotation.*;

@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface AuthIgnore {
    /**
     * 是否忽略所有权限校验
     */
    boolean ignoreAll() default false;

    /**
     * 是否忽略认证校验
     */
    boolean ignoreAuthentication() default false;

    /**
     * 是否忽略授权校验
     */
    boolean ignoreAuthorization() default false;
}
