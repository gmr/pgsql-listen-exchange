%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2015 AWeber Communications
%% @end
%%==============================================================================

%% @doc Constants and records for the pgsql-listen exchange
%% @end
-define(X_TYPE, <<"x-pgsql-listen">>).
-define(X_DESC, <<"PostgreSQL LISTEN Exchange.">>).

-define(VALIDATE_ERROR,
        "Cannot bind a PgSQL Listen exchange to another PgSQL Listen exchange").

-define(DEFAULT_HOST, <<"localhost">>).
-define(DEFAULT_PORT, 5432).
-define(DEFAULT_USER, <<"postgres">>).
-define(DEFAULT_PASSWORD, <<"">>).
-define(DEFAULT_DBNAME, <<"postgres">>).

-record(pgsql_listen_conn, {pid, server, dbname}).
-record(pgsql_listen_dsn, {host, port, user, password, dbname}).
-record(pgsql_listen_state, {amqp, channels, pgsql}).
-record(properties, {app_id, content_type, content_encoding, delivery_mode,
                     headers, priority, reply_to, type}).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("epgsql/include/epgsql.hrl").
