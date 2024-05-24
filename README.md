# niceauth

## 1.介绍
niceauth：好用的Java权限工具，方便、快捷、开箱即用。

本工具与shiro的注解方式类似，但是更加简单、方便。
shiro：需要提供一大堆配置类，而且会有一些事务失效的问题。
niceauth：抽取注解校验工具，配置及使用很精简，无需配置类，无事务失效问题。

## 2.功能
通过注解的方式校验权限。
### 2.1 是否校验权限

| 方法  | 说明  |  备注 |
| :------------ | :------------ | :------------ |
| authCheckRequired(Method)  | 是否校验权限  | 权限总开关  |
| authcCheckRequired(Method)  | 是否校验认证权限（Authentication）  |  略 |
| permissionCheckRequired(Method)  | 是否校验资源权限  | 略  |
| roleCheckRequired(Method)  | 是否校验角色权限  | 略  |


### 2.2 资源权限校验是否通过

permissionCheckSuccess(Method,Collection<String>)

**参数说明**

参数1：表示对应的Controller方法
参数2：表示当前用户拥有的资源权限集合

**执行逻辑**

根据Method对应方法或类上的@RequirePermission注解上的值，遍历Collection<String>，判断是否有权限。详细如下：
先判断方法，如果有@RequirePermission，则用它判断是否有资源权限；如果方法上没有@RequirePermission，则取类上的@RequirePermission，用它判断是否有资源权限。

### 2.3 资源权限校验是否通过

roleCheckSuccess(Method,Collection<String>)

**参数说明**

参数1：表示对应的Controller方法
参数2：表示当前用户拥有的角色权限集合

**执行逻辑**

根据Method对应方法或类上的@RequireRole注解上的值，遍历Collection<String>，判断是否有权限。详细如下：
先判断方法，如果有@RequireRole，则用它判断是否有角色权限；如果方法上没有@RequireRole，则取类上的@RequireRole，用它判断是否有角色权限。

## 3.快速开始
**1. 引入依赖**

```xml
<dependency>
    <groupId>com.suchtool</groupId>
    <artifactId>niceauth-spring-boot-starter</artifactId>
    <version>{newest-version}</version>
</dependency>
```

**2.使用示例**

```
@RestController
@RequestMapping("order")
public class OrderController {
    @RequirePermission("order:add")
    @ApiOperation(value="增加订单")
    @PostMapping("add")
    public Result add() {
        return new Result<>().message("order:add success");
    }

    // 管理员或者产品管理员有权限
    @RequireRole(value = {"admin", "productManager"}, logic = Logic.OR)
    @ApiOperation(value="编辑订单")
    @PostMapping("edit")
    public Result edit() {
        return new Result<>().message("order:edit success");
    }

    // 此时必须同时满足这两个注解的条件才允许访问
    @RequirePermission("order:view")
    @RequireRole(value = {"admin", "productManager"}, logic = Logic.OR)
    @ApiOperation(value="查看订单")
    @GetMapping("view")
    public Result view() {
        return new Result<>().message("order:view success");
    }
}

```

**3.调用方法**

需要获得java.lang.reflect.Method，然后进行调用。

先注入NiceAuthUtil工具类
```
@Autowired
private NiceAuthUtil niceAuthUtil;
```

使用
```
// 可以通过拦截器、AOP等获得目标method
Method method = null;
// 从数据库中读取本用户的资源权限列表
List<String> permissions = null;

// 检查是否有权限
boolean checkSuccess = niceAuthUtil.permissionCheckSuccess(method, permissions);
```

## 4.注解

| 注解  | 含义  | 示例  |
| :------------ | :------------ | :------------ |
| AuthIgnore  | 不校验权限  | 不校验资源权限：@AuthIgnore(ignoreAuth = false, ignorePermission = true)  |
| RequirePermission  | 需要的资源权限 | 需要订单的添加和编辑资源权限才能访问：@RequirePermission(value = {"order:add", "order:edit}, logic = Logic.OR)  |
| RequireRole  | 需要的角色权限。（） | 例1：需要管理员或者订单管理员角色才能访问：@RequireRole(value = {"admin", "orderManager}, logic = Logic.OR) <br/>例2：需要带admin的角色才能访问：@RequireRole(value = {"*admin*"}, logic = Logic.OR)|


## 5.匹配逻辑

使用Spring自带的AntMatcher进行匹配。使用:进行间隔。

| 匹配符  | 含义  | 示例 |
| :------------ | :------------ | :------------ |
| ?  | 匹配一个字符。这个字符不能是代表路径分隔符的:  | order:? 匹配order:a，不匹配order:ab |
| *  | 匹配0到多个字符。 | order:* 匹配order:add，不匹配order:add:abc |
| \*\*  | 匹配多级权限。  | \*\* 匹配所有权限。order:\*\* 匹配order:add，匹配order:add:ab。 |

## 6.配置

支持spring的标准配置文件（yml、property）。

| 配置项  | 含义  |  默认值 |
| :------------ | :------------ | :------------ |
|  suchtool.niceauth.enable-auth | 启用权限 | true  |
|  suchtool.niceauth.default-check-auth | 默认校验权限（没有注解时是否校验）  | true  |
|  suchtool.niceauth.default-check-authentication | 默认校验权限默认校验认证权限（没有注解时是否校验）  | true  |
|  suchtool.niceauth.default-check-permission | 默认校验资源权限（没有注解时是否校验）  | true  |
|  suchtool.niceauth.default-check-role | 默认校验角色权限（没有注解时是否校验）  | true  |

