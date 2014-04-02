PostgreSQL LISTEN Exchange
==========================
A RabbitMQ exchange type that translates PostgreSQL NOTIFY messages to AMQP
messages and publishes them to bound queues.

Notes
-----

- The exchange name should match the PostgreSQL channel notifications are sent to.
- Queues should be bound to the exchange with the channel name as well.
- By default, the plugin connects to PostgreSQL as postgres, on localhost port 5432 to the postgres database.

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
    git clone https://github.com/gmr/rabbitmq-pgsql-listen-exchange.git
    cd rabbitmq-pgsql-listen-exchange
    make

Todo
----

- Close PostgreSQL connections when all bindings are removed
- Persist AMQP connections per exchange
- Add PostgreSQL server information to headers (host, port, dbname)
- Rename configuration variables to use x-pgsql prefix
- Investigate adding policy ability
- Document configuration
- Build binary distributions for RabbitMQ versions
- Move to mirrored supervisor
