-module(system_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-compile(export_all).

all() ->
    [message_published].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    rabbit_ct_helpers:run_setup_steps(
        Config,
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

message_published(Config) ->
    PostgresHost = os:getenv("POSTGRES_HOST", "localhost"),
    PostgresPort = list_to_integer(os:getenv("POSTGRES_PORT", "5432")),

    {ok, Conn} = epgsql:connect(#{
        host => PostgresHost,
        port => PostgresPort,
        username => "postgres",
        password => "password",
        database => "postgres",
        timeout => 4000
    }),
    lager:info("pgsql_test_suite connected to pgsql: ~p", [Conn]),

    Ch = rabbit_ct_client_helpers:open_channel(Config, 0),
    #'exchange.declare_ok'{} =
        amqp_channel:call(Ch, #'exchange.declare'{
            exchange = <<"pgsql-listen-test">>,
            type = <<"x-pgsql-listen">>,
            arguments = [
                {<<"x-host">>, longstr, list_to_binary(PostgresHost)},
                {<<"x-port">>, longstr, integer_to_binary(PostgresPort)},
                {<<"x-dbname">>, longstr, <<"postgres">>},
                {<<"x-user">>, longstr, <<"postgres">>},
                {<<"x-password">>, longstr, <<"password">>}
            ]
        }),

    #'queue.declare_ok'{} =
        amqp_channel:call(Ch, #'queue.declare'{
            queue = <<"pgsql-listen-test">>
        }),

    #'queue.bind_ok'{} =
        amqp_channel:call(Ch, #'queue.bind'{
            queue = <<"pgsql-listen-test">>,
            exchange = <<"pgsql-listen-test">>,
            routing_key = <<"test">>
        }),

    amqp_channel:subscribe(
        Ch,
        #'basic.consume'{
            queue = <<"pgsql-listen-test">>,
            no_ack = true
        },
        self()
    ),

    CTag =
        receive
            #'basic.consume_ok'{consumer_tag = CT} -> CT
        end,

    {ok, _, _} = epgsql:squery(Conn, "NOTIFY test, 'test-value';"),

    Payload =
        receive
            {#'basic.deliver'{}, #amqp_msg{payload = P}} -> P
        after 4000 -> exit(not_received)
        end,

    amqp_channel:call(Ch, #'basic.cancel'{consumer_tag = CTag}),

    ?assertEqual(<<"test-value">>, Payload),

    #'queue.delete_ok'{} =
        amqp_channel:call(Ch, #'queue.delete'{queue = <<"pgsql-listen-test">>}),
    #'exchange.delete_ok'{} =
        amqp_channel:call(Ch, #'exchange.delete'{exchange = <<"pgsql-listen-test">>}),
    rabbit_ct_client_helpers:close_channel(Ch),
    ok.
