-module(rabbitmq_pgsql_listen_exchange).

-include("rabbitmq_pgsql_listen.hrl").
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
   {mfa,         {rabbit_sup, start_child, [rabbitmq_pgsql_listen_sup]}},
   {mfa,         {rabbit_registry, register, [exchange, ?X_TYPE, ?MODULE]}},
   {requires,    rabbit_registry},
   {enables,     kernel_ready}]}).

% ------------------------------------
% Exchange Methods exposed to RabbitMQ
% ------------------------------------

add_binding(Tx, X, B) ->
  gen_server:cast(rabbitmq_pgsql_listen, {add_binding, Tx, X, B}),
  rabbit_exchange_type_direct:add_binding(Tx, X, B).

assert_args_equivalence(X, Args) ->
  rabbit_exchange:assert_args_equivalence(X, Args).

create(_, X) ->
case gen_server:call(rabbitmq_pgsql_listen, {create, X}) of
  ok -> ok;
  {error, Reason} ->
    rabbit_misc:protocol_error(resource_error,
                               "postgresql connection failed: ~s",
                               [Reason])
end.

description() ->
  [{name, ?X_TYPE}, {description, ?X_DESC}].

delete(none, _X, _Bs) ->
  ok;

delete(Tx, X, Bs) ->
  gen_server:cast(rabbitmq_pgsql_listen, {delete, Tx, X, Bs}),
  ok.

policy_changed(X1, X2) ->
  gen_server:cast(rabbitmq_pgsql_listen, {policy_changed, X1, X2}),
  ok.

recover(X, Bs) ->
  rabbit_log:info("rabbitmq-pgsql-listen-exchange recover: ~p, ~p~n", [X, Bs]),
  create(none, X).

remove_bindings(Tx, X, Bs) ->
  gen_server:cast(rabbitmq_pgsql_listen, {remove_bindings, Tx, X, Bs}),
  rabbit_exchange_type_direct:remove_bindings(Tx, X, Bs).

route(X, Delivery) ->
  rabbit_exchange_type_direct:route(X, Delivery).

serialise_events() ->
  false.

validate(X) ->
  case gen_server:call(rabbitmq_pgsql_listen, {validate, X}) of
    ok -> ok;
    {error, Reason} ->
      rabbit_misc:protocol_error(resource_error,
                                 "postgresql connection failed: ~s",
                                 [Reason])
  end.

validate_binding(_X, _B) ->
  ok.
