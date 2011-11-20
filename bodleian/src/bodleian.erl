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
-export([get_manifest_list/1,
         get_manifest/2,
         put_manifest/2,
         update_manifest/3,
         delete_manifest/2,
         get_file/2,
         put_file/2,
         update_file/3,
         delete_file/2]).

%%
%% API Functions
%%
get_manifest_list(User) ->
    {ok, Pid} = bds_connection:create(1000),
    {ok, ManifestList} = bds_connection:get_manifest_list(Pid, User),
    bds_connection:delete(Pid),
    {ok, ManifestList}.

get_manifest(Id, User) ->
    {ok, Pid} = bds_connection:create(),
    {ok, Manifest} = bds_connection:get_manifest(Pid, Id, User),
    bds_connection:delete(Pid),
    {ok, Manifest}.

put_manifest(ManifestData, User) ->
    {ok, Pid} = bds_connection:create(),
    bds_connection:put_manifest(Pid, ManifestData, User),
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

put_file(FileData, User) ->
    {ok, Pid} = bds_connection:create(),
    bds_connection:put_file(Pid, FileData, User),
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

