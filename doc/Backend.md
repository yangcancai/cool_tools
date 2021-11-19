# Backend

### 1. 创建backend callback模块

```erlang
-module(cool_tools_backend_rdbms).

-author("yangcancai").

-export([]).

-callback init(Args :: term()) -> ok.
-callback start() -> ok.
-callback call() -> ok.

-optional_callbacks([call/0]).

```

### 2. 实现真正的模块

```erlang
%% mysql
-module(cool_tools_backend_rdbms_mysql).

-author("yangcancai").

-behaviour(cool_tools_backend_rdbms).

-export([init/1, start/0, call/0]).

init(A) ->
    {mysql_init, A}.

start() ->
    mysql_start.

call() ->
    mysql_call.
```

```erlang

%% pgsql
-module(cool_tools_backend_rdbms_pgsql).

-author("yangcancai").

-behaviour(cool_tools_backend_rdbms).

-export([init/1, start/0, call/0]).

init(A) ->
    {pgsql_init, A}.

start() ->
    pgsql_start.

call() ->
    pgsql_call.
```

### 3. 创建backend模块

```erlang
  {ok, cool_tools_backend_rdbms_backend} =
        cool_tools_backend:create(cool_tools_backend_rdbms, mysql, []),
    mysql = cool_tools_backend_rdbms_backend:backend_name(),
    cool_tools_backend_rdbms_mysql = cool_tools_backend_rdbms_backend:backend(),
```

### 4. 调用

```erlang
run() ->
    {mysql_init, a} = cool_tools_backend_rdbms_backend:init(a),
    %% 切换为pgsql
   {ok, cool_tools_backend_rdbms_backend} =
        cool_tools_backend:create(cool_tools_backend_rdbms, pgsql, []),
    
    {pgsql_init, a} = cool_tools_backend_rdbms_backend:init(a),
     ok.
```