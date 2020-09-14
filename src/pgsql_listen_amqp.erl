%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2020 AWeber Communications
%% @end
%%==============================================================================

%% @doc Abstraction of AMQP related functionality
%% @end

-module(pgsql_listen_amqp).

-export([
    open/1,
    close/2,
    publish/5
]).

-include("pgsql_listen.hrl").

%% @spec open(VHost) -> Result
%% @where
%%       VHost  = binary()
%%       Result = {pid(), pid()}|{error, Reason}
%% @doc Start an internal RabbitMQ connection for the specified virtual host
%% @end
%%
open(VHost) ->
    AdapterInfo = #amqp_adapter_info{name = <<"pgsql_listen_worker">>},
    case
        amqp_connection:start(#amqp_params_direct{
            virtual_host = VHost,
            adapter_info = AdapterInfo
        })
    of
        {ok, Connection} ->
            {ok, Channel} = amqp_connection:open_channel(Connection),
            {ok, Connection, Channel};
        {error, Error} ->
            {error, Error}
    end.

%% @spec close(Connection, Channel) -> ok.
%% where
%%      Connection = pid()
%%      Channel    = pid()
%% @doc Close a connection and its channel
%% @end
%%
close(Connection, Channel) ->
    amqp_channel:call(Channel, #'channel.close'{}),
    amqp_connection:close(Connection),
    ok.

%% @spec publish(Channel, X, Key, Body, Properties) -> Result
%% @where
%%       Channel    = pid()
%%       X          = binary()
%%       Key        = binary()
%%       Body       = binary()
%%       Properties = #properties
%%       Result  = ok | {error, Reason}
%% @doc Publish a message to RabbitMQ using the internal channel
%% @end
%%
publish(Channel, X, Key, Body, Properties) ->
    case
        amqp_channel:call(
            Channel,
            #'basic.publish'{exchange = X, routing_key = Key},
            #amqp_msg{props = properties(Properties), payload = Body}
        )
    of
        ok -> ok;
        Other -> {error, Other}
    end.

%% @private
properties(Properties) ->
    P1 = #'P_basic'{
        app_id = <<"pgsql-listen-exchange">>,
        delivery_mode = Properties#properties.delivery_mode,
        headers = Properties#properties.headers,
        timestamp = erlang:system_time(seconds)
    },
    P2 = maybe_add_content_encoding(P1, Properties),
    P3 = maybe_add_content_type(P2, Properties),
    P4 = maybe_add_priority(P3, Properties),
    P5 = maybe_add_reply_to(P4, Properties),
    maybe_add_type(P5, Properties).

%% @private
maybe_add_content_encoding(Properties, #properties{content_encoding = Value}) when Value =/= null ->
    Properties#'P_basic'{content_encoding = Value};
maybe_add_content_encoding(Properties, _) ->
    Properties.

%% @private
maybe_add_content_type(Properties, #properties{content_type = Value}) when Value =/= null ->
    Properties#'P_basic'{content_type = Value};
maybe_add_content_type(Properties, _) ->
    Properties.

%% @private
maybe_add_priority(Properties, #properties{priority = Value}) when Value =/= null ->
    Properties#'P_basic'{priority = Value};
maybe_add_priority(Properties, _) ->
    Properties.

%% @private
maybe_add_reply_to(Properties, #properties{reply_to = Value}) when Value =/= null ->
    Properties#'P_basic'{reply_to = Value};
maybe_add_reply_to(Properties, _) ->
    Properties.

%% @private
maybe_add_type(Properties, #properties{type = Value}) when Value =/= null ->
    Properties#'P_basic'{type = Value};
maybe_add_type(Properties, _) ->
    Properties.
