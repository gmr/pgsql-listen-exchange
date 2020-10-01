%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2020 AWeber Communications
%% @end
%%==============================================================================

%% @doc Supervisor for the PgSQL LISTEN Exchange worker
%% @end

-module(pgsql_listen_sup).

-behaviour(mirrored_supervisor).

-export([init/1, start_link/0]).

-rabbit_boot_step(
    {pgsql_listen_sup, [
        {description, "pgsql-listen-exchange Supervisor"},
        {mfa, {rabbit_sup, start_child, [?MODULE]}},
        {requires, direct_client},
        {enables, rabbit_exchange_type_pgsql_listen},
        {cleanup, {?MODULE, stop, []}}
    ]}
).

start_link() ->
    mirrored_supervisor:start_link(
        {local, ?MODULE},
        ?MODULE,
        fun rabbit_misc:execute_mnesia_transaction/1,
        ?MODULE,
        []
    ).

init([]) ->
    {ok,
        {
            {one_for_one, 3, 10},
            [
                {
                    pgsql_listen_worker,
                    {pgsql_listen_worker, start_link, []},
                    permanent,
                    5000,
                    worker,
                    [pgsql_listen_worker]
                }
            ]
        }}.
