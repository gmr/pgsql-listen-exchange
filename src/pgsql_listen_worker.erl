%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2020 AWeber Communications
%% @end
%%==============================================================================

%% @doc gen_server process for listening to casts and calls from
%% pgsql_listen_exchange and epgsql
%% @end

-module(pgsql_listen_worker).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_interval/1,
    terminate/2,
    code_change/3
]).

-include("pgsql_listen.hrl").

% How long to wait when starting up to see if rabbit_direct_client_sup exists
-define(DEFERRAL_WAIT, 2000).

% -------------------------
% Worker Startup
% -------------------------

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    rabbit_log:info("starting pgsql-listen exchange worker.~n"),
    register(pgsql_listen_worker, self()),
    {ok, #pgsql_listen_state{amqp = dict:new(), channels = dict:new(), pgsql = dict:new()}}.

% -------------------------
% Gen Server Implementation
% -------------------------

code_change(_, State, _) ->
    {ok, State}.

handle_call({add_binding, X, B}, _From, State) ->
    case whereis(rabbit_direct_client_sup) of
        undefined ->
            defer_call({add_binding, X, B}),
            {reply, ok, State};
        _ ->
            case pgsql_listen_lib:add_binding(X, B, State) of
                {ok, NState} -> {reply, ok, NState};
                {error, Error} -> {reply, {error, Error}, {state, State}}
            end
    end;
handle_call({create, X}, _From, State) ->
    case whereis(rabbit_direct_client_sup) of
        undefined ->
            defer_call({create, X}),
            {reply, ok, State};
        _ ->
            case pgsql_listen_lib:start_exchange(X, State) of
                {ok, NewState} ->
                    {reply, ok, NewState};
                {error, Error} ->
                    {reply, {error, Error}, {state, State}}
            end
    end;
handle_call({delete, X, Bs}, _From, State) ->
    case pgsql_listen_lib:remove_bindings(X, Bs, State) of
        {ok, NState1} ->
            case pgsql_listen_lib:stop_exchange(X, NState1) of
                {ok, NState2} ->
                    {reply, ok, NState2};
                {error, Error} ->
                    {reply, {error, Error}, NState1}
            end;
        {error, Error} ->
            {reply, {error, Error}, State}
    end;
handle_call({remove_bindings, X, Bs}, _From, State) ->
    case pgsql_listen_lib:remove_bindings(X, Bs, State) of
        {ok, NState} -> {reply, ok, NState};
        {error, Error} -> {reply, {error, Error}, {state, State}}
    end;
handle_call({validate, X}, _From, State) ->
    case pgsql_listen_lib:validate_pgsql_connection(X) of
        ok -> {reply, ok, State};
        {error, Error} -> {reply, {error, Error}, State}
    end;
handle_call(_Msg, _From, State) ->
    {noreply, unknown_command, State}.

handle_cast(Cast, State) ->
    rabbit_log:error("pgsql_listen_worker unknown_cast: ~p, ~p~n", [Cast, State]),
    {noreply, State}.

handle_info({epgsql, Conn, {notice, Error}}, State) ->
    rabbit_log:error("pgsql_listen_worker postgres error: ~p, ~p~n", [Conn, Error]),
    {noreply, State};
handle_info({epgsql, Conn, {notification, Channel, _, Payload}}, State) ->
    pgsql_listen_lib:publish_notification(Conn, Channel, Payload, State),
    {noreply, State};
handle_info(Message, State) ->
    rabbit_log:error("pgsql_listen_worker unknown_info: ~p~n", Message),
    {noreply, State}.

terminate(_, _) ->
    ok.

% -----------------------------
% Defer any calls to the worker
% -----------------------------

defer_call(Command) ->
    timer:apply_after(?DEFERRAL_WAIT, ?MODULE, handle_interval, [Command]).

handle_interval(Command) ->
    gen_server:call(?MODULE, Command).
