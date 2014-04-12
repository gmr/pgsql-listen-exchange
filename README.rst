PostgreSQL LISTEN Exchange
==========================
A RabbitMQ exchange type that translates PostgreSQL NOTIFY messages to AMQP
messages and publishes them to bound queues. The PostgreSQL NOTIFY message channel
is used as the routing key for the message using direct exchange style routing
mechanics.

Example
-------
To publish to an exchange named ``test`` and queues bound to the exchange with
a routing key of ``test``, run the following command in psql:

..  code-block:: sql

    postgres=# NOTIFY test, 'This is a test';

Download
--------
To download the pgsql-listen exchange, select the appropriate file that matches
the RabbitMQ version you are running:

+---------+-----------------------------------+
| Version | URL                               |
+=========+===================================+
+---------+-----------------------------------+

Installation
------------
Extract the contents of the zip file into your RabbitMQ plugins directory. Once
extracted, run ``rabbitmq-plugins enable pgsql-listen-exchange``.

Configuration
-------------

**Argument Based Configuration**

To connect to PostgreSQL using something other than the default
``pgsql://postgres@localhost:5432/postgres`` connection, you can
add arguments when declaring the exchange:

+--------------+--------------------------------------+-----------+
| Setting      | Description                          | Data Type |
+==============+======================================+===========+
| x-host       | The PostgreSQL server hostname       | String    |
+--------------+--------------------------------------+-----------+
| x-port       | The port to connect on               | Number    |
+--------------+--------------------------------------+-----------+
| x-dbname     | The database name to connect to      | String    |
+--------------+--------------------------------------+-----------+
| x-user       | The user to connect as               | String    |
+--------------+--------------------------------------+-----------+
| x-password   | The password to use when connecting  | String    |
+--------------+--------------------------------------+-----------+

**Policy Based Configuration**

To apply configuration via a policy, the following settings are available:

+-------------------------+--------------------------------------+-----------+
| Setting                 | Description                          | Data Type |
+=========================+======================================+===========+
| pgsql-listen-host       | The PostgreSQL server hostname       | String    |
+-------------------------+--------------------------------------+-----------+
| pgsql-listen-port       | The port to connect on               | Number    |
+-------------------------+--------------------------------------+-----------+
| pgsql-listen-dbname     | The database name to connect to      | String    |
+-------------------------+--------------------------------------+-----------+
| pgsql-listen-user       | The user to connect as               | String    |
+-------------------------+--------------------------------------+-----------+
| pgsql-listen-password   | The password to use when connecting  | String    |
+-------------------------+--------------------------------------+-----------+


**Configuration in rabbitmq.config**

You can also change the default connection values in the ``rabbitmq.config`` file:

+--------------+--------------------------------------+-----------+---------------+
| Setting      | Description                          | Data Type | Default Value |
+==============+======================================+===========+===============+
| host         | The PostgreSQL server hostname       | list      | "localhost"   |
+--------------+--------------------------------------+-----------+---------------+
| port         | The port to connect on               | integer   | 5432          |
+--------------+--------------------------------------+-----------+---------------+
| dbname       | The database name to connect to      | list      | "postgres"    |
+--------------+--------------------------------------+-----------+---------------+
| user         | The user to connect as               | list      | "postgres"    |
+--------------+--------------------------------------+-----------+---------------+
| password     | The password to use when connecting  | list      | ""            |
+--------------+--------------------------------------+-----------+---------------+

*Exaple rabbitmq.config*

..  code-block:: erlang

    [{pgsql_listen_exchange,
      [
        {host: "localhost"},
        {port: 5432},
        {dbname: "postgres"},
        {user: "postgres"},
        {password: ""},
      ]}
    ].

Building
--------
Steps to custom build a version of the pgsql-listen exchange plugin:

.. code-block:: bash

    hg clone http://hg.rabbitmq.com/rabbitmq-public-umbrella
    cd rabbitmq-public-umbrella
    make co
    make BRANCH=rabbitmq_v3_2_4 up_c
    git clone https://github.com/gmr/epgsql-wrapper.git
    cd epgsql-wrapper
    ./build.sh
    cd ..
    git clone https://github.com/aweber/pgsql-listen-exchange.git
    cd rabbitmq-pgsql-listen-exchange
    make

Todo
----

- Persist AMQP connections per exchange
- Build binary distributions for RabbitMQ versions
