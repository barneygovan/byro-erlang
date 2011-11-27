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
         create_user/1,
         delete_user/1,
         get_manifest/2,
         create_manifest/2,
         get_file/2,
         create_file/2,
         log_error/1,
         log_response/1]).

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

create_user(User) ->
    gen_event:notify(?SERVER, {create_user, User}).

delete_user(User) ->
    gen_event:notify(?SERVER, {delete_user, User}).

get_manifest(Id, User) ->
    gen_event:notify(?SERVER, {get_manifest, {Id, User}}).

create_manifest(Id, User) ->
    gen_event:notify(?SERVER, {create_manifest, {Id, User}}).

get_file(Id, User) ->
    gen_event:notify(?SERVER, {get_file, {Id, User}}).

create_file(Id, User) ->
    gen_event:notify(?SERVER, {create_file, {Id, User}}).

log_error(Error) ->
    gen_event:notify(?SERVER, {log_error, Error}).

log_response(Response) ->
    gen_event:notify(?SERVER, {log_response, Response}).

%%
%% Local Functions
%%

