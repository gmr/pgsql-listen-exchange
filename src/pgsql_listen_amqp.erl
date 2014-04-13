%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%%==============================================================================

%% @doc Abstraction of AMQP related functionality
%% @end

-module(pgsql_listen_amqp).

-export([open/1,
         close/2,
         publish/2]).

%% @spec open(VHost) -> Result
%% @where
%%       VHost  = binary()
%%       Result = {pid(), pid()}|{error, Reason}
%% @doc Start an internal RabbitMQ connection for the specified virtual host
%% @end
%%
open(VHost) ->
  case amqp_connection:start(#amqp_params_direct{virtual_host=VHost}) of
    {ok, Connection} ->
      {ok, Channel} = amqp_connection:open_channel(Connection),
      {Connection, Channel}.
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

%% @spec publish(Channel, X, Key, Headers, Body) -> Result
%% @where
%%       Channel = pid()
%%       X       = binary()
%%       Key     = binary()
%%       Headers = tuple()
%%       Body    = binary()
%%       Result  = ok | {error, Reason}
%% @doc Publish a message to RabbitMQ using the internal channel
%% @end
%%
pubilsh(Channel, X, Key, Headers, Body) ->
  BasicPublish = #'basic.publish'{exchange=X, routing_key=Key},
  Properties = #'P_basic'{app_id = <<"pgsql-listen-exchange">>,
                          delivery_mode = 1,
                          headers = Headers,
                          timestamp = current_timestamp()},
  case amqp_channel:call(Chan,
                          BasicPublish,
                          #amqp_msg{props=Properties, payload=Body}) of
      ok -> ok;
      Other -> {error, Other}
  end.
