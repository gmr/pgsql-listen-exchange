-module(rabbitmq_pgsql_listen_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 3, 10},
        [{rabbitmq_pgsql_listen_worker,
          {rabbitmq_pgsql_listen_worker, start_link, []},
          permanent, 10000, worker,
          [rabbitmq_pgsql_listen_worker]}
        ]}}.
