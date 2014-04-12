
-define(X_TYPE, <<"x-pgsql-listen">>).
-define(X_DESC, <<"PostgreSQL LISTEN Exchange.">>).

-define(VALIDATE_ERROR,
        "Cannot bind a PgSQL Listen exchange to another PgSQL Listen exchange").

-define(DEFAULT_HOST, <<"localhost">>).
-define(DEFAULT_PORT, 5432).
-define(DEFAULT_USER, <<"postgres">>).
-define(DEFAULT_PASS, <<"">>).
-define(DEFAULT_DBNAME, <<"postgres">>).
