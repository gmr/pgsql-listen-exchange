-module(pgsql_listen_exchange).

-include("pgsql_listen.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

-behaviour(rabbit_exchange_type).

-export([add_binding/3,
         assert_args_equivalence/2,
         create/2,
         description/0,
         delete/3,
         policy_changed/2,
         recover/2,
         route/2,
         remove_bindings/3,
         serialise_events/0,
         validate/1,
         validate_binding/2]).

-rabbit_boot_step({?MODULE,
  [{description, ?X_DESC},
   {mfa,         {rabbit_registry, register, [exchange, ?X_TYPE, ?MODULE]}},
   {requires,    rabbit_registry},
   {enables,     recovery}]}).

% ------------------------------------
% Exchange Methods exposed to RabbitMQ
% ------------------------------------

add_binding(none, X, B) ->
  gen_server:cast(pgsql_listen, {add_binding, X, B}),
  ok;

add_binding(_, _, _) ->
  ok.

assert_args_equivalence(X, Args) ->
  rabbit_exchange:assert_args_equivalence(X, Args).

create(none, X) ->
  case gen_server:call(pgsql_listen, {create, X}) of
    ok -> ok;
    {error, Reason} ->
      rabbit_log:error("postgresql connection failed: ~s", [Reason]),
      rabbit_misc:protocol_error(resource_error,
                                 "postgresql connection failed: ~s",
                                 [Reason])
  end;

create(_, _) ->
  ok.

description() ->
  [{name, ?X_TYPE}, {description, ?X_DESC}].

delete(none, X, Bs) ->
  gen_server:call(pgsql_listen, {delete, X, Bs});

delete(_, _, _) ->
  ok.

policy_changed(OldX = #exchange{name = Name}, NewX) ->
  Bs = rabbit_binding:list_for_source(OldX),
  case gen_server:call(pgsql_listen, {delete, OldX, Bs}) of
    ok ->
      create(none, NewX);
    Else ->
      Else
  end.

recover(_, _) ->
  ok.

remove_bindings(none, X, Bs) ->
  gen_server:cast(pgsql_listen, {remove_bindings, X, Bs}),
  ok;

remove_bindings(_, _, _) ->
  ok.

route(X, Delivery) ->
  rabbit_exchange_type_topic:route(X, Delivery).

serialise_events() ->
  false.

validate(X) ->
  case gen_server:call(pgsql_listen, {validate, X}) of
    ok -> ok;
    {error, Reason} ->
      rabbit_log:error("postgresql connection failed: ~s", [Reason]),
      rabbit_misc:protocol_error(resource_error,
                                 "postgresql connection failed: ~s",
                                 [Reason])
  end.

validate_binding(_X, _B) ->
  ok.
