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
	%% Load configuration
	case load_configuration() of
        file_not_found ->
            io:format("No ini file found.~n"),
            {error, "No ini file found."};
        ok ->
            %% TODO: start any services we need running
            ok = startup_required_services([inets]),
            %% TODO: make sure document store is available
            ok = locate_document_store(),
            case bds_sup:start_link() of
            {ok, Pid} ->
                %% Initialize logger
                bds_event_logger:add_handler(),
                {ok, Pid};
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
	
locate_document_store() ->
    ensure_document_store_version().

ensure_document_store_version() ->
    ok.

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
