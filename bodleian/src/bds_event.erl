%% Author: barney
%% Created: Nov 15, 2011
%% Description: TODO: Add description to bds_event
-module(bds_event).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/0,
         add_handler/2,
         delete_handler/2,
         get_manifest/2,
         put_manifest/2,
         get_file/2,
         put_file/2]).

-define(SERVER, ?MODULE).

%%
%% API Functions
%%
start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

get_manifest(Id, User) ->
    gen_event:notify(?SERVER, {get_manifest, {Id, User}}).

put_manifest(Id, User) ->
    gen_event:notify(?SERVER, {put_manifest, {Id, User}}).

get_file(Id, User) ->
    gen_event:notify(?SERVER, {get_file, {Id, User}}).

put_file(Id, User) ->
    gen_event:notify(?SERVER, {put_file, {Id, User}}).

%%
%% Local Functions
%%

