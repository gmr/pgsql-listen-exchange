-module(rabbitmq_pgsql_listen_lib).

-export([get_pgsql_client/2, pgsql_listen/4, publish_message/4]).

-include("rabbitmq_pgsql_listen.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("epgsql/include/pgsql.hrl").

get_pgsql_client(X, Cs) ->
  get_pgsql_connection(Cs, X).

pgsql_listen(Conn, X, B, Xs) ->
  {ok, _, _} = pgsql:squery(Conn, "LISTEN " ++ binary_to_list(B#binding.key)),
  case lists_find({Conn, B#binding.key, X}, Xs) of
    true ->
      {ok, Xs};
    false ->
      {ok, lists:append([Xs, [{Conn, B#binding.key, X}]])}
  end.

publish_message(Conn, Rk, Body, Xs) ->
  X = find_exchange(Conn, Rk, Xs),
  {resource, VHost, exchange, Name} = X#exchange.name,
  {Connection, Channel} = get_amqp_connection(VHost),
  {Host, Port, _, _, DB} = get_pgsql_params(X#exchange.arguments),
  C = lists:flatten([Host, ":", integer_to_list(Port)]),
  BasicPublish = #'basic.publish'{exchange=Name, routing_key=Rk},
  Properties = #'P_basic'{app_id = <<"rabbitmq-pgsql-listen-exchange">>,
                          delivery_mode = 1,
                          headers = [{<<"server">>, longstr, list_to_binary(C)},
                                     {<<"database">>, longstr, list_to_binary(DB)}],
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

get_connection_key(Host, Port, User, Pass, DB) ->
  list_to_atom(lists:concat([Host, ".", integer_to_list(Port), ".", User, ".", Pass, ".", DB])).

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

get_pgsql_connection(Cs, X) ->
  {Host, Port, User, Pass, DB} = get_pgsql_params(X#exchange.arguments),
  Key = get_connection_key(Host, Port, User, Pass, DB),
  case dict:find(Key, Cs) of
    {ok, Conn} ->
      {ok, Cs, Conn};
    error ->
      case create_pgsql_client(Host, Port, User, Pass, DB) of
        {ok, Conn} ->
          New = dict:store(Key, Conn, Cs),
          {ok, New, Conn};
        {error, {{_, {error, Reason}}, _}} ->
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

find_exchange(_, _, []) ->
    false;

find_exchange(Conn, RK, [{Conn, Key, Value} | ListTail]) ->
    case (RK == Key) of
        true when Conn == Conn ->
          Value;
        _ ->
          find_exchange(Conn, RK, ListTail)
    end.

lists_find(_, []) ->
    false;

lists_find(Element, [Item | ListTail]) ->
    case (Item == Element) of
        true ->
          true;
        false ->
          lists_find(Element, ListTail)
    end.
