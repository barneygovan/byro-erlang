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
         create_manifest/4,
         update_manifest/4,
         delete_manifest/3,
         get_file/3,
         create_file/3,
         update_file/4,
         delete_file/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_TIMEOUT, 3000).
-define(DEFAULT_PORT, 5984).
-define(DEFAULT_HOST, "127.0.0.1").
-define(MANIFEST_QUERY, "get_manifest_list").

-record(state, {timeout, host, port}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    Timeout = 
        case bodleian_config:get("couchdb", "timeout") of
            none ->
                ?DEFAULT_TIMEOUT;
            AnyTimeout ->
                AnyTimeout
        end,
    start_link(Timeout).

start_link(Timeout) ->
    Host = 
        case bodleian_config:get("couchdb", "host") of
            none ->
                ?DEFAULT_HOST;
            AnyHost ->
                AnyHost
        end,
    Port = 
        case bodleian_config:get("couchdb", "port") of
            none ->
                ?DEFAULT_PORT;
            AnyPort ->
                AnyPort
        end,
    start_link(Host, Port, Timeout).

start_link(Host, Port) ->
    Timeout = 
        case bodleian_config:get("couchdb", "timeout") of
            none ->
                ?DEFAULT_TIMEOUT;
            AnyTimeout ->
                AnyTimeout
        end,
    start_link(Host, Port, Timeout).

start_link(Host, Port, Timeout) ->
    gen_server:start_link(?MODULE, [Timeout, Host, Port], []).

create() ->
    Timeout = 
        case bodleian_config:get("couchdb", "timeout") of
            none ->
                ?DEFAULT_TIMEOUT;
            AnyTimeout ->
                AnyTimeout
        end,
    create(Timeout).

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

create_manifest(Pid, Id, ManifestData, User) ->
    gen_server:call(Pid, {create_manifest, {Id, ManifestData, User}}).

update_manifest(Pid, Id, ManifestData, User) ->
    gen_server:cast(Pid, {update_manifest, {Id, ManifestData, User}}).

delete_manifest(Pid, Id, User) ->
    gen_server:cast(Pid, {delete_manifest, {Id, User}}).

get_file(Pid, Id, User) ->
    gen_server:call(Pid, {get_file, {Id, User}}).

create_file(Pid, FileData, User) ->
    gen_server:call(Pid, {create_file, {FileData, User}}).

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
    {ok, Response} = http:request(put, {Url, [], "application/json", []}, [], []),
    case handle_response(Response) of
        ok ->
            ViewUrl = create_view_url(State#state.host, State#state.port, User),
            {ok, ViewResponse} = 
                http:request(put, {ViewUrl, [], "application/json", jsondoc_utils:create_user_views()}, [], []),
            case handle_response(ViewResponse) of
                ok -> 
                    {reply, ok, State, State#state.timeout};
                {error, Error} ->
                    ErrorMsg = io_lib:format("~s: ~s", [Error, Url]),
                    {reply, {error, ErrorMsg}, State, State#state.timeout}
            end;
        {error, Error} ->
            ErrorMsg = io_lib:format("~s: ~s", [Error, Url]),
            {reply, {error, ErrorMsg}, State, State#state.timeout}
    end;
handle_call({delete_user, User}, _From, State) ->
    Url = create_url(State#state.host, State#state.port, User),
    bds_event:delete_user(Url),
    {ok, Response} = http:request(delete, {Url, []}, [], []),
    case handle_response(Response) of
        ok -> 
            {reply, ok, State, State#state.timeout};
        {error, Error} ->
            ErrorMsg = io_lib:format("~s: ~s", [Error, Url]),
            {reply, {error, ErrorMsg}, State, State#state.timeout}
    end;
handle_call({create_manifest, {Id, ManifestData, User}}, _From, State) ->
    Url = create_url(State#state.host, State#state.port, User, Id),
    bds_event:create_manifest(Id, Url),
    JsonData = rfc4627:encode(ManifestData),
    JsonDoc = rfc4627:encode(jsondoc_utils:add_header("", manifest, JsonData)),
    Headers = ["Content-Type: application/json"],
    {ok, Response} = http:request(put, {Url, Headers, "application/json", JsonDoc}, [], []),
    case handle_response(Response) of
        ok ->
            {reply, ok, State, State#state.timeout};
        {error, Error} ->
            ErrorMsg = io_lib:format("~s: ~s", [Error, Url]),
            bds_event:log_error(ErrorMsg),
            {reply, {error, ErrorMsg}, State, State#state.timeout}
    end;
handle_call({create_file, {FileData, User}}, _From, State) ->
    Url = create_url(State#state.host, State#state.port, User),
    JsonData = rfc4627:encode(FileData),
    JsonDoc = rfc4627:encode(jsondoc_utils:add_header("", file, JsonData)),
    Headers = ["Content-Type: application/json"],
    {ok, {_Result, _Headers, Body} = Response} = http:request(post, {Url, Headers, "application/json", JsonDoc}, [], []),
    case handle_response(Response) of
        ok ->
            {ok, DecodedBody, _Raw} = rfc4627:decode(Body),
            Id = jsondoc_utils:get_file_id(DecodedBody),
            bds_event:create_file(Id, Url),
            {reply, {ok, Id}, State, State#state.timeout};
        {error, Error} ->
            ErrorMsg = io_lib:format("~s: ~s", [Error, Url]),
            bds_event:log_error(ErrorMsg),
            {reply, {error, ErrorMsg}, State, State#state.timeout}
    end;
handle_call({get_manifest_list, User}, _From, State) ->
    Url = create_view_query_url(State#state.host, State#state.port, User, ?MANIFEST_QUERY),
    {ok, {_Result, _Headers, Body} = Response} = http:request(get, {Url, []}, [], []),
    case handle_response(Response) of
        ok ->
            {ok, DecodedBody, _Raw} = rfc4627:decode(Body),
            ManifestList = 
                jsondoc_utils:process_manifest_list(jsondoc_utils:get_docs_from_couchdb_response(DecodedBody),[]),
            {reply, {ok, ManifestList}, State, State#state.timeout};
        {error, Error} ->
            ErrorMsg = io_lib:format("~s: ~s", [Error, Url]),
            bds_event:log_error(ErrorMsg),
            {reply, {error, ErrorMsg}, State, State#state.timeout}
    end;
handle_call({get_manifest, {Id, User}}, _From, State) ->
    Url = create_url(State#state.host, State#state.port, User, Id),
    bds_event:get_manifest(Id, Url),
    {ok, {_Result, _Headers, Body} = Response} = http:request(get, {Url, []}, [], []),
    case handle_response(Response) of
        ok ->
            {ok, DecodedBody, _Raw} = rfc4627:decode(Body),
            Manifest = jsondoc_utils:strip_header(DecodedBody),
            {reply, {ok, Manifest}, State, State#state.timeout};
        {error, Error} ->
            ErrorMsg = io_lib:format("~s: ~s", [Error, Url]),
            bds_event:log_error(ErrorMsg),
            {reply, {error, ErrorMsg}, State, State#state.timeout}
    end;
%% TODO: get_file code is the same as get_manifest, with only the event handling as a difference
handle_call({get_file, {Id, User}}, _From, State) ->
    Url = create_url(State#state.host, State#state.port, User, Id),
    bds_event:get_file(Id, Url),
    {ok, {_Result, _Headers, Body} = Response} = http:request(get, {Url, []}, [], []),
    case handle_response(Response) of
        ok ->
            {ok, DecodedBody, _Raw} = rfc4627:decode(Body),
            File = jsondoc_utils:strip_header(DecodedBody),
            {reply, {ok, File}, State, State#state.timeout};
        {error, Error} ->
            ErrorMsg = io_lib:format("~s: ~s", [Error, Url]),
            bds_event:log_error(ErrorMsg),
            {reply, {error, ErrorMsg}, State, State#state.timeout}
    end.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(delete, State) ->
    {stop, normal, State};
handle_cast({update_manifest, {Id, ManifestData, User}}, State) ->
    {noreply, State, State#state.timeout};
handle_cast({delete_manifest, {Id, User}}, State) ->
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

create_view_url(Host, Port, Database) ->
    lists:flatten(io_lib:format("http://~s:~w/~s/_design/bodleian", [Host, Port, Database])).

create_view_query_url(Host, Port, Database, Query) ->
    lists:flatten(io_lib:format("http://~s:~w/~s/_design/bodleian/_view/~s", [Host, Port, Database, Query])).

handle_response({{Version, StatusCode, Result}, Headers, Body} ) ->
    
    bds_event:log_response([{version, Version}, {status, StatusCode}, 
                            {result,Result}, {headers, Headers}, {body, Body}]),
    case StatusCode of
        200 -> 
            ok;
        201 ->
            ok;
        202 ->
            ok;
        304 ->
            ok;
        400 ->
            {error, "Incorrect syntax or could not be processed"};
        404 ->
            {error, "No such document"};
        405 ->
            {error, "Incorrect method in request"};
        409 ->
            {error, "A file of the same name already exists"};
        412 ->
            {error, "A user of the same name already exists"};
        500 ->
            {error, "An error occurred in the database"};
        _ ->
            {error, "An unknown error occurred"}
    end.