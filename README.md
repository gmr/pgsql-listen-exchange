PostgreSQL LISTEN Exchange
==========================
A RabbitMQ exchange type that translates PostgreSQL NOTIFY messages to AMQP
messages and publishes them to bound queues.

Notes
-----

- The routing key used to bind an exchange is used to match the channel name the notification is sent to in PostgreSQL.
- By default, the plugin connects to PostgreSQL as postgres, on localhost port 5432 to the postgres database. To change this, see the configuration section below.

Example
-------

To publish to an exchange named `test` and queues bound to the exchange with a routing key of `test`, run the following command in psql:

    postgres=# NOTIFY test, 'This is a test';

Building
--------

    hg clone http://hg.rabbitmq.com/rabbitmq-public-umbrella
    cd rabbitmq-public-umbrella
    make co
    make BRANCH=rabbitmq_v3_2_4 up_c
    git clone https://github.com/gmr/epgsql-wrapper.git
    cd epgsql-wrapper
    ./build.sh
    cd ..
    git clone https://github.com/aweber/rabbitmq-pgsql-listen-exchange.git
    cd rabbitmq-pgsql-listen-exchange
    make

Configuration
-------------
To connect to PostgreSQL using something other than the default
pgsql://postgres@localhost:5432/postgres connection, you can add arguments
when declaring the exchange:

- x-host: The hostname of the PostgreSQL server
- x-port: The port to connect on
- x-dbname: The database name to connect to
- x-user: The user to connect as
- x-password: The password to connect with

You can also change the default connection values in the rabbitmq.config file:

    [{rabbitmq_pgsql_listen_exchange,
      [
        {host: "localhost"},
        {port: 5432},
        {dbname: "postgres"},
        {user: "postgres"},
        {password: ""},
      ]}
    ].

Todo
----

- Close PostgreSQL connections when all bindings are removed
- Persist AMQP connections per exchange
- Investigate adding policy ability
- Build binary distributions for RabbitMQ versions
- Move to mirrored supervisor
