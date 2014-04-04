-module(rabbitmq_pgsql_listen_lib).

-export([close_pgsql_client/2,
         get_pgsql_client/2,
         pgsql_listen/4,
         pgsql_unlisten/4,
         publish_notification/4]).

-include("rabbitmq_pgsql_listen.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("epgsql/include/pgsql.hrl").

close_pgsql_client(X, Cs) ->
  case dict:find(X#exchange.name, Cs) of
    {ok, Conn} ->
      pgsql:close(Conn),
      dict:erase(X#exchange.name, Cs);
    error ->
      Cs
    end.

get_pgsql_client(X, Cs) ->
  {Host, Port, User, Pass, DB} = get_pgsql_params(X#exchange.arguments),
  case dict:find(X#exchange.name, Cs) of
    {ok, Conn} ->
      {ok, Cs, Conn};
    error ->
      case create_pgsql_client(Host, Port, User, Pass, DB) of
        {ok, Conn} ->
          NCs = dict:store(X#exchange.name, Conn, Cs),
          {ok, NCs, Conn};
        {error, {{_, {error, Reason}}, _}} ->
          {error, Reason}
      end
  end.

pgsql_listen(X, B, Cs, Xs) ->
  case dict:find(X#exchange.name, Cs) of
    {ok, Conn} ->
      {ok, _, _} = pgsql:squery(Conn, "LISTEN " ++ binary_to_list(B#binding.key)),
      case lists_find({Conn, B#binding.key, X}, Xs) of
        true ->
          {ok, Xs};
        false ->
          {ok, lists:append([Xs, [{Conn, B#binding.key, X}]])}
      end;
    error ->
      rabbit_log:error("Did not find pgsql connection for ~p~n", [X#exchange.name]),
      error
  end.

pgsql_unlisten(X, B, Cs, Xs) ->
  case dict:find(X#exchange.name, Cs) of
    {ok, Conn} ->
      case lists_find({Conn, B#binding.key, X}, Xs) of
        true ->
          {ok, _, _} = pgsql:squery(Conn, "UNLISTEN " ++ binary_to_list(B#binding.key)),
          {ok, [E || E <- Xs, E =/= {Conn, B#binding.key, X}]};
        false ->
          error
      end;
  error ->
    rabbit_log:error("Did not find pgsql connection for ~p~n", [X#exchange.name]),
    error
end.

publish_notification(Conn, Rk, Body, Xs) ->
  X = [X || {_, R, X} <- [{C, R, X} || {C, R, X} <- Xs, C =:= Conn], R == Rk],
  publish_message(Rk, Body, X).

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

get_pgsql_params(Args) ->
  Host = get_param(Args, "host", ?DEFAULT_HOST),
  Port = list_to_integer(get_param(Args, "port", ?DEFAULT_PORT)),
  User = get_param(Args, "user", ?DEFAULT_USER),
  Pass = get_param(Args, "password", ?DEFAULT_PASS),
  DBName = get_param(Args, "dbname", ?DEFAULT_DBNAME),
  {Host, Port, User, Pass, DBName}.

lists_find(_, []) ->
    false;

lists_find(Element, [Item | ListTail]) ->
    case (Item == Element) of
        true ->
          true;
        false ->
          lists_find(Element, ListTail)
    end.

publish_message(_Rk, _B, []) ->
  false;

publish_message(Rk, Body, [X|ListTail]) ->
  {resource, VHost, exchange, Name} = X#exchange.name,
  {Conn, Chan} = get_amqp_connection(VHost),
  {Host, Port, _, _, DB} = get_pgsql_params(X#exchange.arguments),
  C = lists:flatten([Host, ":", integer_to_list(Port)]),
  BasicPublish = #'basic.publish'{exchange=Name, routing_key=Rk},
  Properties = #'P_basic'{app_id = <<"rabbitmq-pgsql-listen-exchange">>,
                          delivery_mode = 1,
                          headers = [{<<"server">>, longstr, list_to_binary(C)},
                                     {<<"database">>, longstr, list_to_binary(DB)},
                                     {<<"channel">>, longstr, Rk}],
                          timestamp = current_timestamp()},
  amqp_channel:call(Chan,
                    BasicPublish,
                    #amqp_msg{props=Properties, payload=Body}),
  amqp_channel:call(Chan, #'channel.close'{}),
  amqp_connection:close(Conn),
  publish_message(Rk, Body, ListTail).
