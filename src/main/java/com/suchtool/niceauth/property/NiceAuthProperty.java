package com.suchtool.niceauth.property;

import lombok.Data;

@Data
public class NiceAuthProperty {
    /**
     * 启用权限
     */
    private Boolean enableAuth = true;

    /**
     * 默认校验权限（没有注解时是否校验）
     */
    private Boolean defaultCheckAuth = true;

    /**
     * 默认校验认证权限（没有注解时是否校验）
     */
    private Boolean defaultCheckAuthentication = true;

    /**
     * 默认校验资源权限（没有注解时是否校验）
     */
    private Boolean defaultCheckPermission = true;

    /**
     * 默认校验角色权限（没有注解时是否校验）
     */
    private Boolean defaultCheckRole = true;
}
