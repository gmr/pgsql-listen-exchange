%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2015 AWeber Communications
%% @end
%%==============================================================================

%% @doc define the runtime parameters and validators to setup policies
%% @end

-module(pgsql_listen_parameters).

-behaviour(rabbit_policy_validator).

-export([register/0,
         unregister/0,
         validate_policy/1]).

-define(RUNTIME_PARAMETERS,
        [{policy_validator,  <<"pgsql-listen-host">>},
         {policy_validator,  <<"pgsql-listen-port">>},
         {policy_validator,  <<"pgsql-listen-dbname">>},
         {policy_validator,  <<"pgsql-listen-user">>},
         {policy_validator,  <<"pgsql-listen-password">>}]).

-rabbit_boot_step({?MODULE,
                   [{description, "pgsql-listen parameters"},
                    {mfa, {?MODULE, register, []}},
                    {requires, rabbit_registry},
                    {cleanup, {pgsql_listen_parameters, unregister, []}},
                    {enables, recovery}]}).

register() ->
  [rabbit_registry:register(Class, Name, ?MODULE) ||
      {Class, Name} <- ?RUNTIME_PARAMETERS],
  ok.

unregister() ->
    [rabbit_registry:unregister(Class, Name) ||
        {Class, Name} <- ?RUNTIME_PARAMETERS],
    ok.

validate_policy(KeyList) ->
  Host     = proplists:get_value(<<"pgsql-listen-host">>, KeyList, none),
  Port     = proplists:get_value(<<"pgsql-listen-port">>, KeyList, none),
  DBName   = proplists:get_value(<<"pgsql-listen-dbname">>, KeyList, none),
  User     = proplists:get_value(<<"pgsql-listen-user">>, KeyList, none),
  Password = proplists:get_value(<<"pgsql-listen-password">>, KeyList, none),
  Validation = [pgsql_listen_lib:validate_pgsql_host(Host),
                pgsql_listen_lib:validate_pgsql_port(Port),
                pgsql_listen_lib:validate_pgsql_dbname(DBName),
                pgsql_listen_lib:validate_pgsql_user(User),
                pgsql_listen_lib:validate_pgsql_password(Password)],
  case Validation of
    [ok, ok, ok, ok, ok]                   -> ok;
    [{error, Error, Args}, _, _, _, _]     -> {error, Error, Args};
    [ok, {error, Error, Args}, _, _, _]    -> {error, Error, Args};
    [ok, ok, {error, Error, Args}, _, _]   -> {error, Error, Args};
    [ok, ok, ok, {error, Error, Args}, _]  -> {error, Error, Args};
    [ok, ok, ok, ok, {error, Error, Args}] -> {error, Error, Args}
  end.
