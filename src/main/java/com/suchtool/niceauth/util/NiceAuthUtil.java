package com.suchtool.niceauth.util;

import com.suchtool.niceauth.annotation.AuthIgnore;
import com.suchtool.niceauth.annotation.RequirePermission;
import com.suchtool.niceauth.constant.AuthType;
import com.suchtool.niceauth.constant.Logic;
import com.suchtool.niceauth.property.NiceAuthProperty;
import org.springframework.util.AntPathMatcher;

import java.lang.reflect.Method;
import java.util.Collection;

public class NiceAuthUtil {
    private NiceAuthProperty niceAuthProperty;

    private final AntPathMatcher pathMatcher = new AntPathMatcher(":");

    public NiceAuthUtil(NiceAuthProperty niceAuthProperty) {
        this.niceAuthProperty = niceAuthProperty;
    }

    /**
     * 是否需要校验Authentication
     *
     * @param method Controller方法
     * @return 是否需要校验
     */
    public boolean authcCheckRequired(Method method) {
        return checkRequired(method, AuthType.AUTHENTICATION);
    }

    /**
     * 是否需要校验功能权限
     *
     * @param method Controller方法
     * @return 是否需要校验
     */
    public boolean permissionCheckRequired(Method method) {
        return checkRequired(method, AuthType.PERMISSION);
    }

    /**
     * 功能权限是否校验通过
     *
     * @param method Controller方法
     * @return 是否校验通过
     */
    public boolean permissionCheckSuccess(Method method,
                                          Collection<String> permissions) {
        if (!permissionCheckRequired(method)) {
            return true;
        }

        boolean requirePermissionPresent = method.isAnnotationPresent(RequirePermission.class);
        if (!requirePermissionPresent) {
            return false;
        } else {
            RequirePermission requirePermission = method.getAnnotation(RequirePermission.class);
        }

        return false;
    }

    private boolean permissionCheckSuccess(RequirePermission requirePermission,
                                           Collection<String> permissions) {
        String[] values = requirePermission.value();
        if (Logic.OR.equals(requirePermission.logic())) {
            for (String value : values) {
                if (permissionCheckSuccess(value, permissions)) {
                    return true;
                }
            }
            return false;
        } else if (Logic.AND.equals(requirePermission.logic())) {
            for (String value : values) {
                if (!permissionCheckSuccess(value, permissions)) {
                    return false;
                }
            }

            return true;
        } else {
            throw new RuntimeException("不支持此逻辑运算：" + requirePermission.logic());
        }
    }

    private boolean permissionCheckSuccess(String permissionOfAnnotation,
                                           Collection<String> permissions) {
        for (String permission : permissions) {
            if (pathMatcher.match(permission, permissionOfAnnotation)) {
                return true;
            }
        }

        return false;
    }

    private boolean checkRequired(Method method,
                                  AuthType authType) {
        Class<?> declaringClass = method.getDeclaringClass();

        boolean methodAnnotationPresent = method.isAnnotationPresent(AuthIgnore.class);
        if (methodAnnotationPresent) {
            // 方法上有注解，则以方法上的为主

            AuthIgnore authIgnore = method.getAnnotation(AuthIgnore.class);
            if (authIgnore.ignoreAll()) {
                return false;
            } else {
                return doCheckRequired(authIgnore, authType);
            }
        } else {
            // 方法上没注解，则查看类上的注解以及公共配置

            boolean classAnnotationPresent = declaringClass.isAnnotationPresent(AuthIgnore.class);
            if (!classAnnotationPresent) {
                // 如果类上也没注解，取公共配置

                switch (authType) {
                    case AUTHENTICATION:
                        return niceAuthProperty.getDefaultCheckAuthentication();
                    case PERMISSION:
                        return niceAuthProperty.getDefaultCheckPermission();
                    default:
                        throw new RuntimeException("不支持此类型：" + authType);
                }
            } else {
                // 如果类上有注解，则取类的注解值
                AuthIgnore authIgnore = declaringClass.getAnnotation(AuthIgnore.class);
                return doCheckRequired(authIgnore, authType);
            }
        }
    }

    private boolean doCheckRequired(AuthIgnore authIgnore,
                                    AuthType authType) {
        if (authIgnore.ignoreAll()) {
            return false;
        } else {
            switch (authType) {
                case AUTHENTICATION:
                    return !authIgnore.ignoreAuthentication();
                case PERMISSION:
                    return !authIgnore.ignorePermission();
                default:
                    throw new RuntimeException("不支持此类型：" + authType);
            }
        }
    }
}
