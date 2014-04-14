%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%%==============================================================================

%% @doc Supervisor for the PgSQL LISTEN Exchange worker
%% @end

-module(pgsql_listen_sup).

-behaviour(mirrored_supervisor).

-define(WORKER, pgsql_listen_worker).

-rabbit_boot_step({pgsql_listen_supervisor,
                   [{description, "PgSQL Listen Supervisor"},
                    {mfa,         {rabbit_sup, start_child, [?MODULE]}},
                    {requires,    kernel_ready},
                    {enables,     pgsql_listen_exchange}]}).

-export([init/1, start_link/0]).

-include_lib("rabbit_common/include/rabbit.hrl").

start_link() ->
     mirrored_supervisor:start_link(
       {local, ?MODULE}, ?MODULE,
       fun rabbit_misc:execute_mnesia_transaction/1,
       ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{?WORKER,
           {?WORKER, start_link, []},
           permanent, ?MAX_WAIT, worker, [?WORKER]}]}}.
