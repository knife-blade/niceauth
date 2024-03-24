package com.suchtool.niceauth.util;

import com.suchtool.niceauth.annotation.AuthIgnore;
import com.suchtool.niceauth.annotation.RequirePermission;
import com.suchtool.niceauth.annotation.RequireRole;
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
    public boolean authCheckRequired(Method method) {
        return checkRequired(method, AuthType.AUTH);
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
     * 是否需要校验资源权限
     *
     * @param method Controller方法
     * @return 是否需要校验
     */
    public boolean permissionCheckRequired(Method method) {
        return checkRequired(method, AuthType.PERMISSION);
    }

    /**
     * 是否需要校验角色权限
     *
     * @param method Controller方法
     * @return 是否需要校验
     */
    public boolean roleCheckRequired(Method method) {
        return checkRequired(method, AuthType.ROLE);
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

        boolean methodAnnotationPresent = method.isAnnotationPresent(RequirePermission.class);
        if (methodAnnotationPresent) {
            // 如果方法有注解，就以方法上的为准

            RequirePermission requirePermission = method.getAnnotation(RequirePermission.class);
            return permissionCheckSuccess(requirePermission, permissions);
        } else {
            // 如果方法没注解，就以类上的为准

            Class<?> declaringClass = method.getDeclaringClass();
            boolean classAnnotationPresent = declaringClass.isAnnotationPresent(RequirePermission.class);
            if (!classAnnotationPresent) {
                // 如果没注解，则无权限
                return false;
            } else {
                RequirePermission requirePermission = declaringClass.getAnnotation(RequirePermission.class);
                return permissionCheckSuccess(requirePermission, permissions);
            }
        }
    }

    public boolean roleCheckSuccess(Method method,
                                    Collection<String> roles) {
        if (!roleCheckRequired(method)) {
            return true;
        }

        boolean methodAnnotationPresent = method.isAnnotationPresent(RequireRole.class);
        if (methodAnnotationPresent) {
            // 如果方法有注解，就以方法上的为准

            RequireRole requirePermission = method.getAnnotation(RequireRole.class);
            return roleCheckSuccess(requirePermission, roles);
        } else {
            // 如果方法没注解，就以类上的为准

            Class<?> declaringClass = method.getDeclaringClass();
            boolean classAnnotationPresent = declaringClass.isAnnotationPresent(RequireRole.class);
            if (!classAnnotationPresent) {
                // 如果没注解，则无权限
                return false;
            } else {
                RequireRole requirePermission = declaringClass.getAnnotation(RequireRole.class);
                return roleCheckSuccess(requirePermission, roles);
            }
        }
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

    private boolean roleCheckSuccess(RequireRole requireRole,
                                     Collection<String> roles) {
        String[] values = requireRole.value();
        if (Logic.OR.equals(requireRole.logic())) {
            for (String value : values) {
                if (roleCheckSuccess(value, roles)) {
                    return true;
                }
            }
            return false;
        } else if (Logic.AND.equals(requireRole.logic())) {
            for (String value : values) {
                if (!roleCheckSuccess(value, roles)) {
                    return false;
                }
            }

            return true;
        } else {
            throw new RuntimeException("不支持此逻辑运算：" + requireRole.logic());
        }
    }

    private boolean roleCheckSuccess(String roleOfAnnotation,
                                     Collection<String> roles) {
        for (String role : roles) {
            if (pathMatcher.match(roleOfAnnotation, role)) {
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
            if (authIgnore.ignoreAuth()) {
                return false;
            } else {
                return checkRequired(authIgnore, authType);
            }
        } else {
            // 方法上没注解，则查看类上的注解以及公共配置

            boolean classAnnotationPresent = declaringClass.isAnnotationPresent(AuthIgnore.class);
            if (!classAnnotationPresent) {
                // 如果类上也没注解，取公共配置

                switch (authType) {
                    case AUTH:
                        return niceAuthProperty.getDefaultCheckAuth();
                    case AUTHENTICATION:
                        return niceAuthProperty.getDefaultCheckAuthentication();
                    case PERMISSION:
                        return niceAuthProperty.getDefaultCheckPermission();
                    case ROLE:
                        return niceAuthProperty.getDefaultCheckRole();
                    default:
                        throw new RuntimeException("不支持此类型：" + authType);
                }
            } else {
                // 如果类上有注解，则取类的注解值
                AuthIgnore authIgnore = declaringClass.getAnnotation(AuthIgnore.class);
                return checkRequired(authIgnore, authType);
            }
        }
    }

    private boolean checkRequired(AuthIgnore authIgnore,
                                  AuthType authType) {
        switch (authType) {
            case AUTH:
                return !authIgnore.ignoreAuth();
            case AUTHENTICATION:
                return !authIgnore.ignoreAuthentication();
            case PERMISSION:
                return !authIgnore.ignorePermission();
            case ROLE:
                return !authIgnore.ignoreRole();
            default:
                throw new RuntimeException("不支持此类型：" + authType);
        }

    }
}
