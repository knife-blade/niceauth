package com.suchtool.niceauth.constant;

import lombok.Getter;

@Getter
public enum AuthType {
    AUTHENTICATION("认证权限"),
    PERMISSION("资源权限"),
    ;

    private final String description;

    AuthType(String description) {
        this.description = description;
    }
}