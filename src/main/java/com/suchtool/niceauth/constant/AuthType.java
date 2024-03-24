package com.suchtool.niceauth.constant;

import lombok.Getter;

@Getter
public enum AuthType {
    AUTH("权限"),
    AUTHENTICATION("认证权限"),
    PERMISSION("资源权限"),
    ROLE("角色权限"),
    ;

    private final String description;

    AuthType(String description) {
        this.description = description;
    }
}