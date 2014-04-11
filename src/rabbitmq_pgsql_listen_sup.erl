-module(rabbitmq_pgsql_listen_sup).

-behaviour(mirrored_supervisor).

-define(WORKER, rabbitmq_pgsql_listen_worker).

-rabbit_boot_step({pgsql_listen_supervisor,
                   [{description, "PgSQL Listen Supervisor"},
                    {mfa,         {rabbit_sup, start_child, [?MODULE]}},
                    {requires,    kernel_ready},
                    {enables,     rabbitmq_pgsql_listen_exchange}]}).

-export([init/1]).
-export([start_link/0]).

-include_lib("rabbit_common/include/rabbit.hrl").

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{?WORKER,
           {?WORKER, start_link, []},
           permanent, ?MAX_WAIT, worker, [?WORKER]}]}}.

start_link() ->
     mirrored_supervisor:start_link(
       {local, ?MODULE}, ?MODULE,
       fun rabbit_misc:execute_mnesia_transaction/1,
       ?MODULE, []).
