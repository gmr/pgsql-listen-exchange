PROJECT = pgsql_listen_exchange
PROJECT_DESCRIPTION = RabbitMQ Exchange that publishes messages received from PostgreSQL Notifications.

define PROJECT_ENV
[
	{exchange, <<"x-pgsql-listen">>}
]
endef

DEPS = rabbit_common rabbit amqp_client epgsql
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers ct_helper

dep_ct_helper = git https://github.com/extend/ct_helper.git master
dep_epgsql = git https://github.com/epgsql/epgsql.git 4.4.0

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
