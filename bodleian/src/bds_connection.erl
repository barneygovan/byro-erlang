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
         create/0,
         create/1,
         delete/1,
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
-record(state, {timeout}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    start_link(?DEFAULT_TIMEOUT).

start_link(Timeout) ->
    gen_server:start_link(?MODULE, [Timeout], []).

create() ->
    create(?DEFAULT_TIMEOUT).

create(Timeout) ->
    bds_connection_sup:start_child(Timeout).

delete(Pid) ->
    gen_server:cast(Pid, delete).

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
init([Timeout]) ->
    {ok, #state{timeout=Timeout}, Timeout}.

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

