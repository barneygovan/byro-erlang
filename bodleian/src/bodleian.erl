%% Author: barney
%% Created: Nov 15, 2011
%% Description: TODO: Add description to bodleian
-module(bodleian).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([create_user/1,
         delete_user/1,
         get_manifest_list/1,
         get_manifest/2,
         create_manifest/3,
         update_manifest/3,
         delete_manifest/2,
         get_file/2,
         create_file/2,
         update_file/3,
         delete_file/2]).

%%
%% API Functions
%%
create_user(User) ->
    {ok, Pid} = bds_connection:create(),
    Result = bds_connection:create_user(Pid, User),
    bds_connection:delete(Pid),
    case Result of
        ok -> ok;
        {error, Reason} ->
            bds_event:log_error(Reason),
            error
    end.

delete_user(User) ->
    {ok, Pid} = bds_connection:create(),
    ok = bds_connection:delete_user(Pid, User),
    bds_connection:delete(Pid),
    ok.

get_manifest_list(User) ->
    {ok, Pid} = bds_connection:create(),
    {ok, ManifestList} = bds_connection:get_manifest_list(Pid, User),
    bds_connection:delete(Pid),
    {ok, ManifestList}.

get_manifest(Id, User) ->
    {ok, Pid} = bds_connection:create(),
    {ok, Manifest} = bds_connection:get_manifest(Pid, Id, User),
    bds_connection:delete(Pid),
    {ok, Manifest}.

create_manifest(Id, ManifestData, User) ->
    {ok, Pid} = bds_connection:create(),
    bds_connection:create_manifest(Pid, Id, ManifestData, User),
    bds_connection:delete(Pid),
    ok.

update_manifest(Id, ManifestData, User) ->
    {ok, Pid} = bds_connection:create(),
    bds_connection:update_manifest(Pid, Id, ManifestData, User),
    bds_connection:delete(Pid),
    ok.

delete_manifest(Id, User) ->
    {ok, Pid} = bds_connection:create(),
    bds_connection:delete_manifest(Pid, Id, User),
    bds_connection:delete(Pid),
    ok.

get_file(Id, User) ->
    {ok, Pid} = bds_connection:create(),
    {ok, File} = bds_connection:get_file(Pid, Id, User),
    bds_connection:delete(Pid),
    {ok, File}.

create_file(FileData, User) ->
    {ok, Pid} = bds_connection:create(),
    bds_connection:create_file(Pid, FileData, User),
    bds_connection:delete(Pid),
    ok.

update_file(Id, FileData, User) ->
    {ok, Pid} = bds_connection:create(),
    bds_connection:update_file(Pid, Id, FileData, User),
    bds_connection:delete(Pid),
    ok.

delete_file(Id, User) ->
    {ok, Pid} = bds_connection:create(),
    bds_connection:delete_file(Pid, Id, User),
    bds_connection:delete(Pid),
    ok.


%%
%% Local Functions
%%

