%%% -------------------------------------------------------------------
%%% Author  : barney
%%% Description :
%%%
%%% Created : Nov 15, 2011
%%% -------------------------------------------------------------------
-module(bds_connection).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,
         start_link/1,
         start_link/2,
         start_link/3,
         create/0,
         create/1,
         delete/1,
         create_user/2,
         delete_user/2,
         get_manifest_list/2,
         get_manifest/3,
         put_manifest/3,
         update_manifest/4,
         delete_manifest/3,
         get_file/3,
         put_file/3,
         update_file/4,
         delete_file/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_TIMEOUT, 3000).
-define(DEFAULT_PORT, 5984).
-define(DEFAULT_HOST, "127.0.0.1").

-record(state, {timeout, host, port}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    start_link(?DEFAULT_HOST, ?DEFAULT_PORT, ?DEFAULT_TIMEOUT).

start_link(Timeout) ->
    start_link(?DEFAULT_HOST, ?DEFAULT_PORT, Timeout).

start_link(Host, Port) ->
    start_link(Host, Port, ?DEFAULT_TIMEOUT).

start_link(Host, Port, Timeout) ->
    gen_server:start_link(?MODULE, [Timeout, Host, Port], []).

create() ->
    create(?DEFAULT_TIMEOUT).

create(Timeout) ->
    bds_connection_sup:start_child(Timeout).

delete(Pid) ->
    gen_server:cast(Pid, delete).

create_user(Pid, User) ->
    gen_server:call(Pid, {create_user, User}).

delete_user(Pid, User) ->
    gen_server:call(Pid, {delete_user, User}).

get_manifest_list(Pid, User) ->
    gen_server:call(Pid, {get_manifest_list, User}).

get_manifest(Pid, Id, User) ->
    gen_server:call(Pid, {get_manifest, {Id, User}}).

put_manifest(Pid, ManifestData, User) ->
    gen_server:cast(Pid, {put_manifest, {ManifestData, User}}).

update_manifest(Pid, Id, ManifestData, User) ->
    gen_server:cast(Pid, {update_manifest, {Id, ManifestData, User}}).

delete_manifest(Pid, Id, User) ->
    gen_server:cast(Pid, {delete_manifest, {Id, User}}).

get_file(Pid, Id, User) ->
    gen_server:call(Pid, {get_file, {Id, User}}).

put_file(Pid, FileData, User) ->
    gen_server:cast(Pid, {put_file, {FileData, User}}).

update_file(Pid, Id, FileData, User) ->
    gen_server:cast(Pid, {update_file, {Id, FileData, User}}).

delete_file(Pid, Id, User) ->
    gen_server:cast(Pid, {delete_file, {Id, User}}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Timeout, Host, Port]) ->
    {ok, #state{timeout=Timeout, host=Host, port=Port}, Timeout}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({create_user, User}, _From, State) ->
    Url = create_url(State#state.host, State#state.port, User),
    bds_event:create_user(Url),
    {ok, {{_Version, StatusCode, _Result}, _Headers, _Body}} =
        httpc:request(put, {Url, [], "application/json", []}, [], []),
    case StatusCode of
        201 ->
            {reply, ok, State, State#state.timeout};
        412 ->
            ErrorMsg = io_lib:format("User already exists: ~s", [Url]),
            {reply, {error, ErrorMsg}, State, State#state.timeout};
        _ ->
            ErrorMsg = io_lib:format("Unknown error occurred trying to create user: ~s", [Url]),
            {reply, {error, ErrorMsg}, State, State#state.timeout}
    end;
handle_call({delete_user, User}, _From, State) ->
    Url = create_url(State#state.host, State#state.port, User),
    bds_event:delete_user(Url),
    {ok, {{_Version, StatusCode, _Result}, _Headers, _Body}} = 
        httpc:request(delete, {Url, []}, [], []),
    case StatusCode of
        200 -> 
            {reply, ok, State, State#state.timeout};
        404 ->
            ErrorMsg = io_lib:format("No such user exists: ~s", [Url]),
            {reply, {error, ErrorMsg}, State, State#state.timeout};
        _ ->
            ErrorMsg = io_lib:format("Unknown error occurred trying to delete user: ~s", [Url]),
            {reply, {error, ErrorMsg}, State, State#state.timeout}
    end;
handle_call({get_manifest_list, User}, _From, State) ->
    ManifestList = [],
    {reply, {ok, ManifestList}, State, State#state.timeout};
handle_call({get_manifest, {Id, User}}, _From, State) ->
    {reply, ok, State, State#state.timeout};
handle_call({get_file, {Id, User}}, _From, State) ->
    {reply, ok, State, State#state.timeout}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(delete, State) ->
    {stop, normal, State};
handle_cast({put_manifest, {ManifestData, User}}, State) ->
    {noreply, State, State#state.timeout};
handle_cast({update_manifest, {Id, ManifestData, User}}, State) ->
    {noreply, State, State#state.timeout};
handle_cast({delete_manifest, {Id, User}}, State) ->
    {noreply, State, State#state.timeout};
handle_cast({put_file, {FileData, User}}, State) ->
    {noreply, State, State#state.timeout};
handle_cast({update_file, {Id, FileData, User}}, State) ->
    {noreply, State, State#state.timeout};
handle_cast({delete_file, {Id, User}}, State) ->
    {noreply, State, State#state.timeout}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
create_url(Host, Port, Database) ->
    create_url(Host, Port, Database, []).
create_url(Host, Port, Database, Path) ->
    case Path of
        [] ->
            lists:flatten(io_lib:format("http://~s:~w/~s", [Host, Port, Database]));
        _ ->
            lists:flatten(io_lib:format("http://~s:~w/~s/~s", [Host, Port, Database, Path]))
    end.
