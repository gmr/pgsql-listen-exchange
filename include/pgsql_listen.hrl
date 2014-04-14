%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%%==============================================================================

%% @doc Constantsand records for the pgsql-listen exchange
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

-record(pgsql_listen_dsn, {host, port, user, password, dbname}).
-record(pgsql_listen_state, {pgsql, vhosts}).
-record(pgsql_listen_vhost, {connection, channel, bindings}).
-record(pgsql_listen_binding, {exchange, key}).
