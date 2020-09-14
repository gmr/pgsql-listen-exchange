-module(system_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-compile(export_all).

all() ->
    [
        exchange_created
    ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 = rabbit_ct_helpers:set_config(Config, [
        {pgsql_listen_exchange, [
            {host, os:getenv("POSTGRES_HOST", "localhost")},
            {port, list_to_integer(os:getenv("POSTGRES_PORT", "5432"))},
            {dbname, "postgres"},
            {user, "postgres"},
            {password, "password"}
        ]}
    ]),
    rabbit_ct_helpers:run_setup_steps(
        Config1,
        rabbit_ct_broker_helpers:setup_steps() ++
            rabbit_ct_client_helpers:setup_steps()
    ).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(
        Config,
        rabbit_ct_client_helpers:teardown_steps() ++
            rabbit_ct_broker_helpers:teardown_steps()
    ).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% -------------------------------------------------------------------
%% Testsuite cases
%% -------------------------------------------------------------------

exchange_created(Config) ->
    Ch = rabbit_ct_client_helpers:open_channel(Config),
    X = <<"pgsql-listen-test">>,
    #'exchange.declare_ok'{} =
        amqp_channel:call(Ch, #'exchange.declare'{
            exchange = X,
            type = <<"x-pgsql-listen">>
        }),
    #'exchange.delete_ok'{} =
        amqp_channel:call(Ch, #'exchange.delete'{exchange = X}),
    rabbit_ct_client_helpers:close_channel(Ch),
    ok.
