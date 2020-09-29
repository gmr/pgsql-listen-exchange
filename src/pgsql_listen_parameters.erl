%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2020 AWeber Communications
%% @end
%%==============================================================================

%% @doc define the runtime parameters and validators to setup policies
%% @end

-module(pgsql_listen_parameters).

-behaviour(rabbit_policy_validator).

-export([
    register/0,
    unregister/0,
    validate_policy/1
]).

-define(RUNTIME_PARAMETERS, [
    {policy_validator, <<"pgsql-listen-host">>},
    {policy_validator, <<"pgsql-listen-port">>},
    {policy_validator, <<"pgsql-listen-dbname">>},
    {policy_validator, <<"pgsql-listen-user">>},
    {policy_validator, <<"pgsql-listen-password">>},
    {policy_validator, <<"pgsql-listen-ssl">>}
]).

-rabbit_boot_step(
    {?MODULE, [
        {description, "pgsql-listen parameters"},
        {mfa, {?MODULE, register, []}},
        {requires, rabbit_registry},
        {cleanup, {pgsql_listen_parameters, unregister, []}},
        {enables, recovery}
    ]}
).

register() ->
    [rabbit_registry:register(Class, Name, ?MODULE) || {Class, Name} <- ?RUNTIME_PARAMETERS],
    ok.

unregister() ->
    [rabbit_registry:unregister(Class, Name) || {Class, Name} <- ?RUNTIME_PARAMETERS],
    ok.

validate_policy(KeyList) ->
    Host = proplists:get_value(<<"pgsql-listen-host">>, KeyList, none),
    Port = proplists:get_value(<<"pgsql-listen-port">>, KeyList, none),
    DBName = proplists:get_value(<<"pgsql-listen-dbname">>, KeyList, none),
    User = proplists:get_value(<<"pgsql-listen-user">>, KeyList, none),
    Password = proplists:get_value(<<"pgsql-listen-password">>, KeyList, none),
    Validation = [
        validate_pgsql_host(Host),
        validate_pgsql_port(Port),
        validate_pgsql_dbname(DBName),
        validate_pgsql_user(User),
        validate_pgsql_password(Password)
    ],
    case Validation of
        [ok, ok, ok, ok, ok] -> ok;
        [{error, Error, Args}, _, _, _, _] -> {error, Error, Args};
        [ok, {error, Error, Args}, _, _, _] -> {error, Error, Args};
        [ok, ok, {error, Error, Args}, _, _] -> {error, Error, Args};
        [ok, ok, ok, {error, Error, Args}, _] -> {error, Error, Args};
        [ok, ok, ok, ok, {error, Error, Args}] -> {error, Error, Args}
    end.

%% @private
%% @spec validate_pgsql_dbname(Value) -> Result
%% @where
%%       Value  = binary()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified PostgreSQL dbname is a binary value or none
%% @end
%%
validate_pgsql_dbname(none) ->
    ok;
validate_pgsql_dbname(Value) ->
    validate_binary_or_none("pgsql-listen-dbname", Value).

%% @private
%% @spec validate_pgsql_host(Value) -> Result
%% @where
%%       Value  = binary()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified PostgreSQL hostname is a binary or none
%% @end
%%
validate_pgsql_host(none) ->
    ok;
validate_pgsql_host(Value) ->
    validate_binary_or_none("pgsql-listen-host", Value).

%% @private
%% @spec validate_pgsql_password(Value) -> Result
%% @where
%%       Value  = binary()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified PostgreSQL password is a binary or none
%% @end
%%
validate_pgsql_password(none) ->
    ok;
validate_pgsql_password(Value) ->
    validate_binary_or_none("pgsql-listen-password", Value).

%% @private
%% @spec validate_pgsql_port(Value) -> Result
%% @where
%%       Value  = integer()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified PostgreSQL port is an integer value or none
%% @end
%%
validate_pgsql_port(none) ->
    ok;
validate_pgsql_port(Value) when is_number(Value) ->
    ok;
validate_pgsql_port(Value) ->
    {error, "pgsql-listen-port should be a number, actually was ~p", [Value]}.

%% @private
%% @spec validate_pgsql_user(Value) -> Result
%% @where
%%       Value  = binary()|none
%%       Result = ok|{error, Error}
%% @doc Validate the user specified PostgreSQL user is a binary value or none
%% @end
%%
validate_pgsql_user(none) ->
    ok;
validate_pgsql_user(Value) ->
    validate_binary_or_none("pgsql-listen-user", Value).

%% @private
%% @spec validate_binary_or_none(Name, Value) -> Result
%% @doc Validate the user specified PostgreSQL user is a binary value or none
%% @where
%%       Name   = list()
%%       Value  = binary()|none
%%       Result = ok|{error, Error}
%% @end
%%
validate_binary_or_none(_, none) ->
    ok;
validate_binary_or_none(_, Value) when is_binary(Value) ->
    ok;
validate_binary_or_none(Name, Value) ->
    {error, "~s should be binary, actually was ~p", [Name, Value]}.
