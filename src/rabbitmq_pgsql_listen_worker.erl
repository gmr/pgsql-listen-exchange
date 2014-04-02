-module(rabbitmq_pgsql_listen_worker).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("rabbitmq_pgsql_listen.hrl").

%---------------------------
% Worker Startup
% --------------------------

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
  register(rabbitmq_pgsql_listen, self()),
  rabbit_log:info("rabbitmq-pgsql-listen-exchange worker: started (~p)~n", [self()]),
  {ok,  {state, {dict:new()}}}.

%---------------------------
% Gen Server Implementation
% --------------------------

code_change(_, State, _) ->
  {ok, State}.

handle_call({create, X}, _From, State) ->
  {Response, Conns} = rabbitmq_pgsql_listen_lib:get_pgsql_client(X, State),
  {reply, Response, {state, {Conns}}};

handle_call(_Msg, _From, _State) ->
  {noreply, unknown_command, _State}.

handle_cast({add_binding, Tx, X, B}, State) ->
  rabbit_log:info("add_binding: ~p, ~p, ~p ~p~n", [Tx, X, B, State]),
  {noreply, State};

handle_cast({delete, Tx, X, Bs}, State) ->
  rabbit_log:info("delete: ~p, ~p, ~p ~p~n", [Tx, X, Bs, State]),
  {noreply, State};

handle_cast({policy_changed, X1, X2}, State) ->
  rabbit_log:info("policy_changed: ~p, ~p, ~p~n", [X1, X2, State]),
  {noreply, State};

handle_cast({remove_bindings, Tx, X, Bs}, State) ->
  rabbit_log:info("remove_bindings: ~p, ~p, ~p ~p~n", [Tx, X, Bs, State]),
  {noreply, State};

handle_cast(Cast, State) ->
  rabbit_log:info("Unknown cast: ~p, ~p~n", [Cast, State]),
  {noreply, State}.

handle_info({pgsql, Conn, {notification, Channel, _Pid, Payload}},
            {state, {Connections}}) ->
  case dict:find(Channel, Connections) of
    {ok, {VHost, Conn}} ->
      rabbitmq_pgsql_listen_lib:publish_message(VHost, Channel, Payload);
    error ->
      rabbit_log:info("Couldnt find mapping for ~p: ~s\n", [Channel, Payload])
  end,
  {noreply, {state, {Connections}}};

handle_info(Message, State) ->
  rabbit_log:info("handle_info: ~p~n", Message),
  {noreply, State}.

terminate(_, {state, {Connections}}) ->
  ok.
