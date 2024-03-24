package com.suchtool.niceauth.property;

import lombok.Data;

@Data
public class NiceAuthProperty {
    /**
     * 默认校验认证权限（没有注解时是否校验）
     */
    private Boolean defaultCheckAuthentication;

    /**
     * 默认校验功能权限（没有注解时是否校验）
     */
    private Boolean defaultCheckPermission;
}
