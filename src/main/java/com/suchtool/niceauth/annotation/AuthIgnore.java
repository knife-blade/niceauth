package com.suchtool.niceauth.annotation;

import java.lang.annotation.*;

@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface AuthIgnore {
    /**
     * 是否忽略权限校验。包括：Authentication、Permission、Role、其他。
     */
    boolean ignoreAuth() default false;

    /**
     * 是否忽略认证校验
     */
    boolean ignoreAuthentication() default false;

    /**
     * 是否忽略功能权限校验
     */
    boolean ignorePermission() default false;

    /**
     * 是否忽略角色权限校验
     */
    boolean ignoreRole() default false;
}
