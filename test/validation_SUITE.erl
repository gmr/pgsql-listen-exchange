-module(validation_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [validate_policy_ok_test].

validate_policy_ok_test(_Config) ->
    KeyList = [
        {<<"pgsql-listen-host">>, <<"localhost">>},
        {<<"pgsql-listen-port">>, 5432},
        {<<"pgsql-listen-dbname">>, <<"dbname">>},
        {<<"pgsql-listen-user">>, <<"user">>},
        {<<"pgsql-listen-password">>, <<"password">>}
    ],
    ?assertEqual(ok, pgsql_listen_parameters:validate_policy(KeyList)),
    ok.
