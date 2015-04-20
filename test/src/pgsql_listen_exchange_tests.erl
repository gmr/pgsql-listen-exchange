-module(pgsql_listen_exchange_tests).

-include_lib("eunit/include/eunit.hrl").

validate_policy_ok_test() ->
  KeyList = [{<<"pgsql-listen-host">>, <<"localhost">>},
             {<<"pgsql-listen-port">>, 5432},
             {<<"pgsql-listen-dbname">>, <<"dbname">>},
             {<<"pgsql-listen-user">>, <<"user">>},
             {<<"pgsql-listen-password">>, <<"password">>}],
  ?assertEqual(ok, pgsql_listen_parameters:validate_policy(KeyList)).
