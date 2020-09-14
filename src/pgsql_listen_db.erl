%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014-2020 AWeber Communications
%% @end
%%==============================================================================

%% @doc PostgreSQL database functionality abstraction
%% @end

-module(pgsql_listen_db).

-export([
    close/1,
    connect/1,
    listen/2,
    unlisten/2
]).

-include("pgsql_listen.hrl").

%% @spec close(Conn) -> ok
%% @where
%%       Conn = pid()
%% @doc Close an open PostgreSQL client connection
%% @end
%%
close(Conn) ->
    epgsql:close(Conn).

%% @spec connect(DSN) -> Result
%% @doc Create a new PostgreSQL connection
%% @where
%%       DSN    = pgsql_dsn record
%%       Result = {ok, pid()}|{error, Error}
%% @end
%%
connect(#pgsql_listen_dsn{
    host = Host,
    port = Port,
    user = User,
    password = Password,
    dbname = DBName
}) ->
    epgsql:connect(Host, User, Password, [
        {database, DBName},
        {port, Port},
        {timeout, 2500},
        {async, self()}
    ]).

%% @spec listen(Connection, Channel) -> Result
%% @where
%%       Connection = pid()
%%       Channel    = list()
%%       Result     = ok|{error, Error}
%% @doc Listen to the specified channel
%% @end
%%
listen(Connection, Channel) ->
    query(Connection, "LISTEN " ++ Channel).

%% @spec unlisten(Connection, Channel) -> Result
%% @where
%%       Connection = pid()
%%       Channel    = list()
%%       Result     = ok|{error, Error}
%% @doc Cancel listening to the specified channel
%% @end
%%
unlisten(Connection, Channel) ->
    query(Connection, "UNLISTEN " ++ Channel).

%% @private
%% @spec query(Connection, SQL) -> Result
%% @where
%%       Connection = pid()
%%       Channel    = list()
%%       Result     = ok|{error, Error}
%% @doc Execute the specified SQL query
%% @end
%%
query(Connection, SQL) ->
    case epgsql:squery(Connection, SQL) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {ok, _, _, _} -> ok;
        {error, Error} -> {error, Error}
    end.
