DEPS:=rabbitmq-server rabbitmq-erlang-client epgsql-wrapper
RELEASABLE:=true
STANDALONE_TEST_COMMANDS:=eunit:test([pgsql_listen_exchange_tests],[verbose])
