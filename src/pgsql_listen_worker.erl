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
    terminate/2,
    code_change/3
]).

-include("pgsql_listen.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = #pgsql_listen_state{
        amqp = dict:new(),
        channels = dict:new(),
        pgsql = dict:new()
    },
    {ok, maybe_connect(rabbit_exchange:list(), State)}.

code_change(_, State, _) ->
    {ok, State}.

handle_call({add_binding, X, B}, _From, State) ->
    case pgsql_listen_lib:add_binding(X, B, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Error} ->
            {reply, {error, Error}, {state, State}}
    end;
handle_call({create, X}, _From, State) ->
    case pgsql_listen_lib:start_exchange(X, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Error} ->
            {reply, {error, Error}, {state, State}}
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
    rabbit_log:error("pgsql_listen_worker unknown_cast: ~p", [Cast]),
    {noreply, State}.

handle_info({epgsql, Conn, {notice, Error}}, State) ->
    rabbit_log:error("pgsql_listen_worker postgres error: ~p (~p)", [Conn, Error]),
    {noreply, State};
handle_info({epgsql, Conn, {notification, Channel, _, Payload}}, State) ->
    {noreply, pgsql_listen_lib:publish_notification(Conn, Channel, Payload, State)};
handle_info(Message, State) ->
    rabbit_log:error("pgsql_listen_worker unknown_info: ~p", [Message]),
    {noreply, State}.

terminate(_, _) ->
    ok.

% -------------------------

maybe_connect([X = #exchange{name = Name, type = 'x-pgsql-listen'} | Tail], State) ->
    case pgsql_listen_lib:start_exchange(X, State) of
        {ok, NewState} ->
            maybe_connect(Tail, add_bindings(X, rabbit_binding:list_for_source(Name), NewState));
        {error, Error} ->
            rabbit_log:error(
                "pgsql_listen_exchange startup error for ~p: ~p",
                [Name, Error]
            ),
            maybe_connect(Tail, State)
    end;
maybe_connect([_X | Tail], State) ->
    maybe_connect(Tail, State);
maybe_connect([], State) ->
    State.

add_bindings(X = #exchange{name = Name}, [H | T], State) ->
    case pgsql_listen_lib:add_binding(X, H, State) of
        {ok, NewState} ->
            add_bindings(X, T, NewState);
        {error, Error} ->
            rabbit_log:error(
                "pgsql_listen_exchange error adding binding ~p: ~p",
                [Name, Error]
            ),
            add_bindings(X, T, State)
    end;
add_bindings(_X, [], State) ->
    State.
