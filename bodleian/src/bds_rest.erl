%%% -------------------------------------------------------------------
%%% Author  : barney
%%% Description :
%%%
%%% Created : Feb 19, 2012
%%% -------------------------------------------------------------------
-module(bds_rest).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([handler/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE).
-define(HTTP_OPTS, [
                    {loop, {?MODULE, handler}},
                    {port, 8080},
                    {name, bds_rest_http}]).
-define(HTTPS_OPTS, [
                     {loop, {?MODULE, handler}},
                     {port, 8443},
                     {ssl, true},
                     {ssl_opts, [
                                 {certfile, "server_cert.pem"},
                                 {keyfile, "server_key.pem"}
                                 ]}]).

-record(state, {http, https}).


%% ====================================================================
%% External functions
%% ====================================================================
handler(Request) ->
    case Request:get(method) of
        'GET' ->
            handle_get(Request);
        'PUT' ->
            handle_put(Request);
        'DELETE' ->
            handle_delete(Request);
        _ ->
            Headers = [{"Allow", "GET,PUT,DELETE"}],
            Request:respond({405, Headers, "405 Method Not Allowed\r\n"})
    end.

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
init([]) ->
    {ok, Http} = mochiweb_http:start(?HTTP_OPTS),
    {ok, Https} = mochiweb_http:start(?HTTPS_OPTS),
    State = #state{http=Http, https=Https},
    {ok, State}.

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
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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
handle_get(Request) ->
    Path = Request:get(path),
    Parts = string:tokens(Path, "/"),
    [ResourceType|Parts2] = Parts,
    case ResourceType of 
        manifest ->
            [UserName|ManifestParts] = Parts2,
            [ManifestName|[]] = ManifestParts,
            case bodleian:get_manifest(ManifestName, UserName) of
                {ok, Data} ->
                    ok;
                _ ->
                    %%handle error
                    ok
            end;
        file ->
            [UserName|FileParts] = Parts2,
            [FileName|[]] = FileParts;
        _ ->
            Request:respond({404, [], "404 Not Found\r\n"})
    end.

handle_put(Request) ->
    Path = Request:get(path),
    ok.

handle_delete(Request) ->
    Path = Request:get(path),
    ok.

