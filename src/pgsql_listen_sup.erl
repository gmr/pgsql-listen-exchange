%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2015 AWeber Communications
%% @end
%%==============================================================================

%% @doc Supervisor for the PgSQL LISTEN Exchange worker
%% @end

-module(pgsql_listen_sup).

-behaviour(supervisor2).

-define(WORKER, pgsql_listen_worker).

-export([init/1, start_link/0, stop/0]).

-rabbit_boot_step({pgsql_listen_supervisor,
                   [{description, "PgSQL Listen Supervisor"},
                    {mfa,         {rabbit_sup, start_child, [?MODULE]}},
                    {requires,    kernel_ready},
                    {cleanup,     {?MODULE, stop, []}},
                    {enables,     pgsql_listen_exchange}]}).


-include_lib("rabbit_common/include/rabbit.hrl").

start_link() ->
   rabbit:maybe_insert_default_data(),
   supervisor2:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{?WORKER, {?WORKER, start_link, []},
           transient, ?MAX_WAIT, worker, [?WORKER]}]}}.

stop() ->
    ok = supervisor:terminate_child(rabbit_sup, ?MODULE),
    ok = supervisor:delete_child(rabbit_sup, ?MODULE).
