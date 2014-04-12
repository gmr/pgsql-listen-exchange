-module(pgsql_listen_parameters).

-behaviour(rabbit_runtime_parameter).
-behaviour(rabbit_policy_validator).

-export([register/0,
         notify/4,
         notify_clear/3,
         validate/4,
         validate_policy/1]).

-rabbit_boot_step({?MODULE,
                   [{description, "pgsql-listen parameters"},
                    {mfa, {?MODULE, register, []}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

register() ->
  [rabbit_registry:register(Class, Name, ?MODULE) ||
      {Class, Name} <- [{runtime_parameter, <<"pgsql-listen-host">>},
                        {runtime_parameter, <<"pgsql-listen-port">>},
                        {runtime_parameter, <<"pgsql-listen-dbname">>},
                        {runtime_parameter, <<"pgsql-listen-user">>},
                        {runtime_parameter, <<"pgsql-listen-password">>},
                        {policy_validator,  <<"pgsql-listen-host">>},
                        {policy_validator,  <<"pgsql-listen-port">>},
                        {policy_validator,  <<"pgsql-listen-dbname">>},
                        {policy_validator,  <<"pgsql-listen-user">>},
                        {policy_validator,  <<"pgsql-listen-password">>}]],
  ok.

notify(VHost, What, Name, Term) ->
  rabbit_log:info("notify: ~s,~s,~s,~s~n", [VHost, What, Name, Term]),
  rabbit_policy:notify(VHost, What, Name, Term).

notify_clear(VHost, What, Name) ->
  rabbit_log:info("notify_clear: ~s,~s,~s~n", [VHost, What, Name]),
  rabbit_policy:notify_clear(VHost, What, Name).

validate(VHost, What, Name, Term) ->
  rabbit_log:info("Validating ~s,~s,~s,~s~n", [VHost, What,Name, Term]),
  rabbit_policy:validate(VHost, <<"policy">>, Name, Term).

validate_policy(KeyList) ->
  rabbit_log:info("Validating policy~n"),
  Host     = proplists:get_value(<<"pgsql-listen-host">>, KeyList, none),
  Port     = proplists:get_value(<<"pgsql-listen-port">>, KeyList, none),
  DBName   = proplists:get_value(<<"pgsql-listen-dbname">>, KeyList, none),
  User     = proplists:get_value(<<"pgsql-listen-user">>, KeyList, none),
  Password = proplists:get_value(<<"pgsql-listen-password">>, KeyList, none),
  Validation = [pgsql_listen_lib:validate_host(Host),
                pgsql_listen_lib:validate_port(Port),
                pgsql_listen_lib:validate_dbname(DBName),
                pgsql_listen_lib:validate_user(User),
                pgsql_listen_lib:validate_password(Password)],
  rabbit_log:info("Validation results: ~p~n", [Validation]),
  case Validation of
    [ok, ok, ok, ok, ok]                   -> ok;
    [{error, Error, Args}, _, _, _, _]     -> {error, Error, Args};
    [ok, {error, Error, Args}, _, _, _]    -> {error, Error, Args};
    [ok, ok, {error, Error, Args}, _, _]   -> {error, Error, Args};
    [ok, ok, ok, {error, Error, Args}, _]  -> {error, Error, Args};
    [ok, ok, ok, ok, {error, Error, Args}] -> {error, Error, Args}
  end.
