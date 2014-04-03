-module(rabbitmq_pgsql_listen_lib).

-export([get_pgsql_client/2, publish_message/3]).

-include("rabbitmq_pgsql_listen.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("epgsql/include/pgsql.hrl").

get_pgsql_client(#exchange{arguments=Args, name=Name}, {state, {Connections}}) ->
  {Host, Port, User, Pass, DB} = get_pgsql_params(Args),
  case get_pgsql_connection(Connections, Name, Host, Port, User, Pass, DB) of
    {ok, Conns} ->
      {ok, Conns};
    {error, Reason} ->
      {error, Reason, Connections}
  end.

publish_message(VHost, X, Body) ->
  {Connection, Channel} = get_amqp_connection(VHost),
  BasicPublish = #'basic.publish'{exchange=X, routing_key=X},
  Properties = #'P_basic'{app_id = <<"rabbitmq-pgsql-listen-exchange">>,
                          delivery_mode = 1,
                          timestamp = current_timestamp()},
  amqp_channel:call(Channel,
                    BasicPublish,
                    #amqp_msg{props=Properties, payload=Body}),
  amqp_channel:call(Channel, #'channel.close'{}),
  amqp_connection:close(Connection).

%------------------
% Internal Methods
%------------------

create_pgsql_client(Host, Port, User, [], DB) ->
  pgsql:connect(Host, User, "", [{database, DB}, {port, Port}, {async, self()}]);

create_pgsql_client(Host, Port, User, Pass, DB) ->
  pgsql:connect(Host, User, Pass, [{database, DB}, {port, Port}, {async, self()}]).

convert_gregorian_to_julian(GregorianSeconds) ->
  GregorianSeconds - 719528 * 24 * 3600.

current_gregorian_timestamp() ->
  calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now())).

current_timestamp() ->
  convert_gregorian_to_julian(current_gregorian_timestamp()).

get_amqp_connection(VHost) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_direct{virtual_host=VHost}),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  {Connection, Channel}.

get_env(EnvVar, DefaultValue) ->
  case application:get_env(rabbitmq_pgsql_listen, EnvVar) of
    undefined ->
      DefaultValue;
    {ok, V} ->
      V
  end.

get_param(Args, Name, Default) when is_atom(Name) ->
  get_param_value(Args, atom_to_list(Name), Default);

get_param(Args, Name, Default) when is_list(Name) ->
  get_param_value(Args, Name, Default).

get_param_env_value(Name, Default) when is_atom(Name) ->
  get_env(Name, Default);

get_param_env_value(Name, Default) when is_list(Name) ->
  get_env(list_to_atom(Name), Default).

get_param_list_value(Value) when is_binary(Value) ->
  binary_to_list(Value);

get_param_list_value(Value) when is_integer(Value) ->
  integer_to_list(Value);

get_param_list_value(Value) when is_list(Value) ->
  integer_to_list(Value).

get_param_value(Args, Name, Default) ->
  case lists:keyfind(list_to_binary("x-" ++ Name), 1, Args) of
    {_, _, V} -> get_param_list_value(V);
            _ -> get_param_list_value(get_param_env_value(Name, Default))
  end.

get_pgsql_connection(Connections, X, Host, Port, User, Pass, DB) ->
  {resource, VHost, exchange, Name} = X,
  Channel = binary_to_list(Name),
  case dict:find(Name, Connections) of
    {ok, {VHost, Conn}} ->
      ok = pgsql_listen(Conn, binary_to_list(Name)),
      {ok, Connections};
    error ->
      rabbit_log:info("~p ~p ~p ~p ~p", [Host, Port, User, Pass, DB]),
      case create_pgsql_client(Host, Port, User, Pass, DB) of
        {ok, Conn} ->
          New = dict:store(Name, {VHost, Conn}, Connections),
          ok = pgsql_listen(Conn, Channel),
          {ok, New};
        {error, {{_, {error, Reason}}, _}} ->
          rabbit_log:error("Error connecting to PostgreSQL on ~p:~p as ~p: ~p",
                           [Host, Port, User, Reason]),
          {error, Reason}
      end
  end.

get_pgsql_params(Args) ->
  Host = get_param(Args, "host", ?DEFAULT_HOST),
  Port = list_to_integer(get_param(Args, "port", ?DEFAULT_PORT)),
  User = get_param(Args, "user", ?DEFAULT_USER),
  Pass = get_param(Args, "password", ?DEFAULT_PASS),
  DBName = get_param(Args, "dbname", ?DEFAULT_DBNAME),
  {Host, Port, User, Pass, DBName}.

pgsql_listen(Connection, Channel) ->
  {ok, _, _} = pgsql:squery(Connection, "LISTEN " ++ Channel),
  ok.

select_exchange(Exchange, VHost, Channel) ->
  Name = Exchange#exchange.name,
  case Name of
    {resource, VHost, exchange, Channel} ->
      Exchange;
    _ ->
      rabbit_log:info("Exchange: ~p~n", [Name]),
      null
  end.
