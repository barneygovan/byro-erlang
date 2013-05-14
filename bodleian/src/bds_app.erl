%% Author: barney
%% Created: Nov 15, 2011
%% Description: TODO: Add description to bds_app
-module(bds_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(DEFAULT_INI_FILE, "bds.ini").
-define(MIN_DOC_STORE_VERSION, "1.0.0").
-define(BDS_VERSION, "0.1.0").

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->    
    io:format("-----------------------------------------------~n"),
	io:format("Starting Bodleian Document Server v~s.~n", [?BDS_VERSION]),
	%% Load configuration
	case load_configuration() of
        file_not_found ->
            io:format("No ini file found.~n"),
            {error, "No ini file found."};
        ok ->
            ok = startup_required_services([inets]),
            case ensure_document_store_version() of
				ok ->
		            case bds_sup:start_link() of
		            {ok, Pid} ->
		                %% Initialize logger
		                bds_event_logger:add_handler(),
		                {ok, Pid};
		            Error ->
		                Error
		            end;
				Error ->
					Error
			end
    end.
    

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
load_configuration() ->
	case init:get_argument(bds_ini) of
		{ok, [[IniFilename]]} ->
			bodleian_config:read_config_file(IniFilename, filename);
		_Any ->
			bodleian_config:read_config_file(?DEFAULT_INI_FILE, filename)
	end.

ensure_document_store_version() ->
    io:format("-----------------------------------------------~n"),
    io:format("Locating backend document store~n"),
    case bds_connection:get_version() of 
		{ok, Version} ->
			case bodleian_utils:compare_versions(?MIN_DOC_STORE_VERSION, 
												 binary_to_list(Version), 
												 minimum) of
				false ->
					io:format("Cannot use couchdb version ~s, need minimum of version ~s~n", [Version, ?MIN_DOC_STORE_VERSION]),
					{error, incorrect_couchdb_version};
				true ->
				    io:format("Found couchdb version ~s~n", [Version]),
					ok
			end;
		{error, Error} ->
			io:format("Error connecting to couchdb: ~s~n", [Error]),
			{error, couchdb_connection_error}
	end.

startup_required_services([]) ->
	ok;
startup_required_services([Service|Rest]) ->
    case application:start(Service) of 
		ok ->
			startup_required_services(Rest);
		{error, {already_started, Service}} ->
			startup_required_services(Rest);
		{error, _Reason} ->
			{error, {cant_start, Service}}
	end.
