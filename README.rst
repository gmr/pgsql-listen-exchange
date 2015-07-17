PostgreSQL LISTEN Exchange
==========================
A RabbitMQ exchange type that translates PostgreSQL NOTIFY messages to AMQP
messages and publishes them to bound queues. The PostgreSQL NOTIFY message channel
is used as the routing key for the message using direct exchange style routing
mechanics.

.. image:: https://img.shields.io/travis/aweber/pgsql-listen-exchange.svg
    :target: https://travis-ci.org/aweber/pgsql-listen-exchange
.. image:: https://img.shields.io/github/release/aweber/pgsql-listen-exchange.svg
    :target: https://github.com/aweber/pgsql-listen-exchange/releases

Example
-------
To publish Postgres notifications as AMQP messages into queues bound to a
``x-pgsql-listen`` exchange with a binding key of ``test``, run the following
command in psql:

..  code-block:: sql

    postgres=# NOTIFY test, 'This is a test';

Installation
------------
Extract the contents of the release zip file into your RabbitMQ plugins
directory. Once extracted, run ``rabbitmq-plugins enable pgsql_listen_exchange``.

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

*Example rabbitmq.config*

..  code-block:: erlang

    [{pgsql_listen_exchange,
      [
        {host, "localhost"},
        {port, 5432},
        {dbname, "postgres"},
        {user, "postgres"},
        {password, ""}
      ]}
    ].

Message Properties
------------------
The exchange will automatically the following properties to messages:

+-----------+---------------------------------------------------+
| Property  | Value                                             |
+===========+===================================================+
| app_id    | ``pgsql-listen-exchange``                         |
+-----------+---------------------------------------------------+
| headers   | *See "Headers Properties Values" table below*     |
+-----------+---------------------------------------------------+
| timestamp | The UNIX epoch timestamp of the publishing server |
+-----------+---------------------------------------------------+

Headers Property Values
^^^^^^^^^^^^^^^^^^^^^^^
The following table details the values of the headers property that is set on each message.

+-----------------+-----------------------------------------------------------------+
| Key             | Value                                                           |
+=================+=================================================================+
| pgsql-channel   | The PostgreSQL notification channel                             |
+-----------------+-----------------------------------------------------------------+
| pgsql-server    | The host and port of the PostgreSQL server                      |
+-----------------+-----------------------------------------------------------------+
| source-exchange | The pgsql-listen-exchange that the notification was received by |
+-----------------+-----------------------------------------------------------------+

Specifying Other Properties
^^^^^^^^^^^^^^^^^^^^^^^^^^^
In addition to the automatically set message properties, the exchange can set
configured message properties. To set one of the supported message properties,
specify the property name and value when binding to the exchange. For example,
to set the ``content_type`` property, specify ``content_type`` and the value it
should be set to when binding a queue to the exchange.  The following message
properties are supported:

+------------------+-----------+
| Property         | Data Type |
+==================+===========+
| content_encoding | String    |
+------------------+-----------+
| content_type     | String    |
+------------------+-----------+
| delivery_mode    | Number    |
+------------------+-----------+
| priority         | Number    |
+------------------+-----------+
| reply_to         | String    |
+------------------+-----------+
| type             | String    |
+------------------+-----------+

Building
--------
Steps to custom build a version of the pgsql-listen-exchange plugin:

.. code-block:: bash

    git clone https://github.com/rabbitmq/rabbitmq-public-umbrella
    cd rabbitmq-public-umbrella
    make co
    make BRANCH=rabbitmq_v3_5_4 up_c
    git clone https://github.com/gmr/epgsql-wrapper.git
    git clone https://github.com/aweber/pgsql-listen-exchange.git
    cd rabbitmq-pgsql-listen-exchange
    make
