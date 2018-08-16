-module(ct_data_util).

-define(MNESIADIR, "dbmnesia").
-define(SQLITE, "cargotube.sqlite").

-export([
         stop_mnesia/0,
         start_mnesia/0,
         create_mnesia_schema_if_needed/0,

         setup_sqlite_if_needed/0,
         get_sqlite_connection/0,

         get_table_version/1,
         set_table_version/2
        ]).

stop_mnesia() ->
    application:stop(mnesia),
    MnesiaDir = mnesia_dir(),
    application:set_env(mnesia, dir, MnesiaDir),
    ok.

start_mnesia() ->
    {ok, _} = application:ensure_all_started(mnesia),
    ok.

create_mnesia_schema_if_needed() ->
    Dir = mnesia_dir(),
    DirExists = filelib:is_dir(Dir),
    create_mnesia_schema_if_needed(DirExists).


create_mnesia_schema_if_needed(true) ->
    ok;
create_mnesia_schema_if_needed(false) ->
    ok = stop_mnesia(),
    ok = mnesia:create_schema([node()]),
    ok = start_mnesia(),
    ok.

mnesia_dir() ->
    application:get_env(ct_data_util, mnesia_dir, ?MNESIADIR).


setup_sqlite_if_needed() ->
    IsConfigured = get_sqlite_connection(),
    setup_sqlite_if_needed(IsConfigured).


setup_sqlite_if_needed(undefined) ->
    SqliteFile = application:get_env(ct_data_util, mnesia_dir, ?SQLITE),
    {ok, Connection} = esqlite3:open(SqliteFile),
    Sql = "CREATE TABLE IF NOT EXISTS ctdatainfo ( name TEXT PRIMARY KEY"
        ", version REAL NOT NULL)",
    ok = esqlite3:exec(Sql, Connection),
    application:set_env(ct_data_util, sqlite_connection, Connection),
    ok;
setup_sqlite_if_needed({ok, _}) ->
    ok.


get_table_version(Table) ->
    MaybeCon = get_sqlite_connection(),
    maybe_get_table_version(Table, MaybeCon).

maybe_get_table_version(Table, {ok, Con}) ->
    Sql = "SELECT version FROM ctdatainfo WHERE name = ?",
    Result = esqlite3:q(Sql, [Table], Con),
    handle_version_result(Result);
maybe_get_table_version(_Table, _NoCon) ->
    {error, not_connected}.

handle_version_result([]) ->
    ok;
handle_version_result(Other) ->
    {error, Other}.


set_table_version(Table, Version) ->
    MaybeCon = get_sqlite_connection(),
    maybe_set_table_version(Table, Version, MaybeCon).


maybe_set_table_version(Table,  Version, {ok, Con}) ->
    Sql = "INSERT OR REPLACE INTO ctdatainfo (name, version) VALUES (?, ?)",
    Result = esqlite3:q(Sql, [Table, Version], Con),
    handle_set_version_result(Result);
maybe_set_table_version(_Table, _Version, _NoCon) ->
    {error, not_connected}.

handle_set_version_result([]) ->
    {error, not_found};
handle_set_version_result([{Version}]) ->
    {ok, Version}.


get_sqlite_connection() ->
    application:get_env(ct_data_util, sqlite_connection).
