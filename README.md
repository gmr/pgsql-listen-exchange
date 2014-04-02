RabbitMQ PostgreSQL LISTEN Exchange
===================================
Exchange that translates PostgreSQL NOTIFY messages to AMQP messages
and publishes them to bound queues.

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
