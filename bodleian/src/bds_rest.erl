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
-export([start_link/2,
		 handler/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_HTTP_PORT, 8080).
-define(DEFAULT_HTTPS_PORT, 8443).
-define(DEFAULT_HTTP_OPTS, [
		                    {loop, {?MODULE, handler}},
		                    {port, ?DEFAULT_HTTP_PORT},
		                    {name, bds_rest_http}]).
-define(DEFAULT_HTTPS_OPTS, [
		                     {loop, {?MODULE, handler}},
		                     {port, ?DEFAULT_HTTPS_PORT},
		                     {ssl, true},
		                     {ssl_opts, [
		                                 {certfile, "server_cert.pem"},
		                                 {keyfile, "server_key.pem"}
		                                 ]}]).

-record(state, {http, https}).


%% ====================================================================
%% External functions
%% ====================================================================
start_link(HttpOpts, HttpsOpts) ->
	gen_server:start_link(?MODULE, [HttpOpts, HttpsOpts], []).
	
handler(Request) ->
    case Request:get(method) of
        'GET' ->
			%% get_manifest_list
			%% get_manifest
			%% get_file
            handle_get(Request);
        'PUT' ->
			%% update_manifest
			%% update_file
            handle_put(Request);
		'POST' ->
			%% create_manifest
			%% create_file
			handle_post(Request);
        'DELETE' ->
			%% delete_manifest
			%% delete_file
            handle_delete(Request);
        Other ->
			io:format("Illegal method: ~s~n", [Other]),
            Headers = [{"Allow", "GET,PUT,POST,DELETE"}],
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
init([HttpOpts, HttpsOpts]) ->
	case HttpOpts of
		[] ->
			io:format("Started Listening on Port ~B.~n", [?DEFAULT_HTTP_PORT]),
			{ok, Http} = mochiweb_http:start(?DEFAULT_HTTP_OPTS);
		HttpOpts ->
			io:format("Started Listening on Port ~B.~n", [proplists:lookup(port, HttpOpts)]),
			{ok, Http} = mochiweb_http:start(HttpOpts)
	end,
	case HttpsOpts of
		[] ->
			io:format("Started Secure Listening on Port ~B.~n", [?DEFAULT_HTTPS_PORT]),
    		{ok, Https} = mochiweb_http:start(?DEFAULT_HTTPS_OPTS);
		HttpsOpts ->
			io:format("Started Secure Listening on Port ~B.~n", [proplists:lookup(port, HttpsOpts)]),
    		{ok, Https} = mochiweb_http:start(HttpsOpts)
	end,
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
	io:format("Requesting: GET ~s~n", [Path]),
    Parts = string:tokens(Path, "/"),
    [ResourceType|Parts2] = Parts,
    case ResourceType of 
        "manifest" ->
            [UserName|ManifestParts] = Parts2,
            [ManifestName|[]] = ManifestParts,
			case ManifestName of
				"_list" ->
					case bodleian:get_manifest_list(UserName) of
						{ok, Code, Data} ->
							JsonData = mochijson2:encode({struct, [ {manifests, [M || M <- Data]} ] }),
							Request:respond({Code, [{"Content-Type", "application/json"}], JsonData});
						{error, Code, Error} ->
							Request:respond({Code, [], Error})
					end;
				ManifestName ->
            		case bodleian:get_manifest(ManifestName, UserName) of
                		{ok, Code, Data} ->
                    		Request:respond({Code, [{"Content-Type", "application/json"}], Data});
                		{error, Code, Error} ->
							Request:respond({Code, [], Error})
            		end
			end;
        "file" ->
            [UserName|FileParts] = Parts2,
            [FileName|[]] = FileParts,
			case bodleian:get_file(FileName, UserName) of
				{ok, Code, Data} ->
					Request:respond({Code, [{"Content-Type", "application/json"}], Data});
				{error, Code, Error} ->
					Request:respond({Code, [], Error})
			end;
        Other ->
			io:format("Illegal resource request: ~s~n", [Other]),
            Request:not_found()
    end.

handle_put(Request) ->
    Path = Request:get(path),
	io:format("Requesting: PUT ~s~n", [Path]),
	Parts = string:tokens(Path, "/"),
	[ResourceType|Parts2] = Parts,
	case ResourceType of
		"user" ->
			Request:respond({401, [], <<"Unauthorized">>});
		"manifest" ->
			[UserName|ManifestParts] = Parts2,
			[ManifestName|[]] = ManifestParts,
			
			case bodleian:update_manifest(ManifestName, Request:recv_body(), UserName) of
				{ok, Code} ->
					ok;
				{error, Code, Error} ->
					Request:respond({Code, [], Error})
			end;
		"file" ->
			[UserName|FileParts] = Parts2,
			[FileName|[]] = FileParts,
			ok
	end.


handle_post(Request) ->
	Path = Request:get(path),
	Parts = string:tokens(Path, "/"),
	[ResourceType|Parts2] = Parts,
	case ResourceType of 
		"manifest" ->
			[UserName|ManifestParts] = Parts2,
			[ManifestName|[]] = ManifestParts,
			case bodleian:create_manifest(ManifestName, Request:recv_body(), UserName) of
				{ok, Code} ->
					Request:respond({Code, [], []});
				{error, Code, Error} ->
					Request:respond({Code, [], Error})
			end;
		"file" ->
			[UserName|[]] = Parts2,
			case bodleian:create_file(Request:recv_body(), UserName) of
				{ok, Code} ->
					Request:respond({Code, [], []});
				{error, Code, Error} ->
					Request:respond({Code, [], Error})
			end
	end.

handle_delete(Request) ->
    Path = Request:get(path),
    ok.


