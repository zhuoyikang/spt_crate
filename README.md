本组件提供一些erlang编程或系统工具集合。
d
* spt_reloader-热更新
* spt_notify-事件注册/分发
* spt_smerl-动态模块编程库

### 1.spt_reloader-热更新

热更新进程，来源于michi-web，可以对线上运行的代码进行热替换，但应该遵守以下面规则：

* 新代码必须没有改变上下文数据结构，才能进行热部署.
* 有一个以上模块被修改，模块被reload的顺序不可保证，因此，如果你的函数原型发生了变化，或者调用了新增的函数，热部署都很危险。
* 对在单个模块代码bug的hotfix，非常适合直接reloader。

默认情况下spt_reloader启动，当beam代码发生变化时将会自动热更新。

### 2.spt_notify-事件注册/分发.

游戏中需要关注很多事件的发生，比如建筑升级事件，玩家打赢了一个boss事件，spt_notify提供事件的注册和发生接口，有以下3个api:

* sub(Event, Fun):订阅
* unSub(Event, Fun)：取消订阅
* post(Event, Param):事件发生.

post事件的第2个参数将会被原样传递给注册的函数，使用例子如下：

    Fun1 = fun(X) ->  io:format("x1 ~p~n", [X]) end,
    Fun2 = fun(X) ->  io:format("x2 ~p~n", [X]) end,
    spt_notify:sub(e1,  Fun1),
    spt_notify:sub(e1, Fun2),
    spt_notify:post(e1, 23),
    spt_notify:ubsub(e1, Fun1),
    spt_notify:post(e1, 23),


### 3.spt_smerl-动态模块编程库

虽然erlang的动态编程能力不强(也或者是我学的很浅)，但是smerl这个模块用来做动态模块扩展是比较合适的，它来源于erlyweb项目，已经稳定了很多年。

以下情况适合使用smerl动态产生模块:

* 大量的重复编程模块：比如slg_model里的表model，基本结构都一样(select, update, delete)，只有一点参数的不同而已。
* 环境参数：有的环境参数，如果放在ets表又太慢了，放在固定的模块在每次修改时又需要编译，所以我倾向于动态产生一个模块，然后从模块函数里直接获取配置参数。

使用方法如下，来源于源码注释：

    test_smerl() ->
    M1 = spt_smerl:new(foo),
    {ok, M2} = spt_smerl:add_func(M1, "bar() -> 1 + 1."),
    spt_smerl:compile(M2),
    foo:bar(),   % returns 2``
    spt_smerl:has_func(M2, bar, 0). % returns true

