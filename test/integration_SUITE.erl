-module(integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [
        {group, notifications}
    ].

groups() ->
    [
        {notifications, [], [
            {application_config, [], [notification_received]},
            {exchange_args_config, [], [notification_received]},
            {policy_based_config, [], [notification_received]}
        ]}
    ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(notifications, Config) ->
    Config;
init_per_group(application_config, Config) ->
    Config0 = rabbit_ct_helpers:merge_app_env(
        Config,
        {pgsql_listen_exchange, [
            {host, list_to_binary(postgres_host())},
            {port, postgres_port()},
            {dbname, <<"postgres">>},
            {user, <<"postgres">>},
            {password, <<"password">>}
        ]}
    ),
    init_per_group(all, Config0);
init_per_group(policy_based_config = Group, Config) ->
    Config0 = rabbit_ct_helpers:run_setup_steps(
        Config,
        rabbit_ct_broker_helpers:setup_steps() ++
            rabbit_ct_client_helpers:setup_steps()
    ),
    rabbit_ct_broker_helpers:set_policy(
        Config0,
        0,
        <<"pgsql-listen-test">>,
        <<"pgsql-listen-test">>,
        <<"exchanges">>,
        [
            {<<"pgsql-listen-host">>, list_to_binary(postgres_host())},
            {<<"pgsql-listen-port">>, postgres_port()},
            {<<"pgsql-listen-dbname">>, <<"postgres">>},
            {<<"pgsql-listen-user">>, <<"postgres">>},
            {<<"pgsql-listen-password">>, <<"password">>}
        ]
    ),
    setup_rabbitmq_objects(Group, Config0),
    Config0;
init_per_group(Group, Config) ->
    Config0 = rabbit_ct_helpers:run_setup_steps(
        Config,
        rabbit_ct_broker_helpers:setup_steps() ++
            rabbit_ct_client_helpers:setup_steps()
    ),
    setup_rabbitmq_objects(Group, Config0),
    Config0.

end_per_group(notifications, Config) ->
    Config;
end_per_group(policy_based_config, Config) ->
    rabbit_ct_broker_helpers:clear_policy(Config, 0, <<"pgsql-listen-test">>),
    end_per_group(all, Config);
end_per_group(_, Config) ->
    Channel = rabbit_ct_client_helpers:open_channel(Config, 0),
    #'queue.delete_ok'{} = amqp_channel:call(Channel, #'queue.delete'{
        queue = <<"pgsql-listen-test">>
    }),
    #'exchange.delete_ok'{} = amqp_channel:call(Channel, #'exchange.delete'{
        exchange = <<"pgsql-listen-test">>
    }),
    rabbit_ct_client_helpers:close_channel(Channel),
    rabbit_ct_helpers:run_teardown_steps(
        Config,
        rabbit_ct_client_helpers:teardown_steps() ++
            rabbit_ct_broker_helpers:teardown_steps()
    ).

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% -------------------------------------------------------------------
%% Testsuite cases
%% -------------------------------------------------------------------

notification_received(Config) ->
    Channel = rabbit_ct_client_helpers:open_channel(Config, 0),
    amqp_channel:subscribe(
        Channel,
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
    send_notification("test", "test-value"),
    Payload = wait_for_message(),
    amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = CTag}),
    ?assertEqual(<<"test-value">>, Payload),
    rabbit_ct_client_helpers:close_channel(Channel),
    ok.

%% -------------------------------------------------------------------
%% Helper Functions
%% -------------------------------------------------------------------

declare_exchange(Channel, exchange_args_config) ->
    #'exchange.declare_ok'{} =
        amqp_channel:call(Channel, #'exchange.declare'{
            exchange = <<"pgsql-listen-test">>,
            type = <<"x-pgsql-listen">>,
            arguments = [
                {<<"x-host">>, longstr, postgres_host()},
                {<<"x-port">>, longstr, integer_to_binary(postgres_port())},
                {<<"x-dbname">>, longstr, <<"postgres">>},
                {<<"x-user">>, longstr, <<"postgres">>},
                {<<"x-password">>, longstr, <<"password">>}
            ]
        });
declare_exchange(Channel, _) ->
    #'exchange.declare_ok'{} =
        amqp_channel:call(Channel, #'exchange.declare'{
            exchange = <<"pgsql-listen-test">>,
            type = <<"x-pgsql-listen">>
        }).

postgres_host() ->
    os:getenv("POSTGRES_HOST", "localhost").

postgres_port() ->
    list_to_integer(os:getenv("POSTGRES_PORT", "5432")).

send_notification(Channel, Payload) ->
    {ok, Conn} = epgsql:connect(#{
        host => postgres_host(),
        port => postgres_port(),
        username => "postgres",
        password => "password",
        database => "postgres",
        timeout => 4000
    }),
    {ok, _, _} = epgsql:squery(Conn, "NOTIFY " ++ Channel ++ ", '" ++ Payload ++ "';"),
    epgsql:close(Conn).

setup_rabbitmq_objects(Group, Config) ->
    Channel = rabbit_ct_client_helpers:open_channel(Config, 0),
    declare_exchange(Channel, Group),
    #'queue.declare_ok'{} =
        amqp_channel:call(Channel, #'queue.declare'{
            queue = <<"pgsql-listen-test">>
        }),
    #'queue.bind_ok'{} =
        amqp_channel:call(Channel, #'queue.bind'{
            queue = <<"pgsql-listen-test">>,
            exchange = <<"pgsql-listen-test">>,
            routing_key = <<"test">>
        }),
    rabbit_ct_client_helpers:close_channel(Channel).

wait_for_message() ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Payload}} -> Payload
    after 4000 -> exit(not_received)
    end.
