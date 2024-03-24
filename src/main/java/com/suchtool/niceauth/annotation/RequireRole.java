package com.suchtool.niceauth.annotation;

import com.suchtool.niceauth.constant.Logic;

import java.lang.annotation.*;

@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface RequireRole {
    String[] value();

    Logic logic() default Logic.OR;
}
