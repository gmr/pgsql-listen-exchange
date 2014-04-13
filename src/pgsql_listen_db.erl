%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%% =====================================================================

%% @doc PostgreSQL database functionality abstraction
%% @end


-module(pgsql_listen_db).

-export([close/1,
         connect/5,
         listen/2,
         unlisten/2]).

-include("pgsql_listen.hrl").

-include_lib("epgsql/include/pgsql.hrl").

%% @spec close(Conn) -> ok
%% @where
%%       Conn = pid()
%% @doc Close an open PostgreSQL client connection
%% @end
%%
close(Conn) ->
  pgsql:close(Conn).

%% @spec connect(Host, Port, User, Password, DBName) -> Result
%% @doc Create a new PostgreSQL connection
%% @where
%%       Host     = list()
%%       Port     = integer()
%%       User     = list()
%%       Password = list()
%%       DBName   = list()
%%       Conn     = pid()
%%       Result   = {ok, pid()}|{error, Error}
%% @end
%%
connect(Host, Port, User, Pass, DBName) ->
  pgsql:connect(Host, User, Pass, [{database, DBName},
                                   {port, Port},
                                   {async, self()}]).

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
  case pgsql:squery(Connection, SQL) of
    {ok, _Columns, _Rows} -> ok;
    {error, Error} -> {error, Error}
  end.
