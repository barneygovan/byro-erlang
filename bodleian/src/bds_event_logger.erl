%%% -------------------------------------------------------------------
%%% Author  : barney
%%% Description :
%%%
%%% Created : Nov 15, 2011
%%% -------------------------------------------------------------------
-module(bds_event_logger).

-behaviour(gen_event).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([add_handler/1,
         add_handler/0, 
         delete_handler/0]).

%% gen_event callbacks
-export([init/1, 
         handle_event/2, 
         handle_call/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3]).

-define(DEBUG, true).

-record(state, {debug}).

%% ====================================================================
%% External functions
%% ====================================================================
add_handler(Debug) ->
    bds_event:add_handler(?MODULE, [Debug]).

add_handler() ->
    add_handler(?DEBUG).

delete_handler() ->
    bds_event:delete_handler(?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%% --------------------------------------------------------------------
init([Debug]) ->
    {ok, #state{debug=Debug}}.

%% --------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_event({create_user, User}, State) ->
    error_logger:info_msg("create_user[~s]~n", [User]),
    {ok, State};
handle_event({delete_user, User}, State) ->
    error_logger:info_msg("delete_user[~s]~n", [User]),
    {ok, State};
handle_event({get_manifest, {Id, User}}, State) ->
    error_logger:info_msg("get_manifest[~s, ~s]~n", [Id, User]),
    {ok, State};
handle_event({create_manifest, {Id, User}}, State) ->
    error_logger:info_msg("create_manifest[~s, ~s]~n", [Id, User]),
    {ok, State};
handle_event({get_file, {Id, User}}, State) ->
    error_logger:info_msg("get_file[~s, ~s]~n", [Id, User]),
    {ok, State};
handle_event({create_file, {Id, User}}, State) ->
    error_logger:info_msg("create_file[~s, ~s]~n", [Id, User]),
    {ok, State};
handle_event({log_error, Error}, State) ->
    error_logger:error_msg("ERROR: ~s~n", [Error]),
    {ok, State};
handle_event({log_response, Response}, State) ->
    case State#state.debug of
        true ->
            error_logger:info_report(Response),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% --------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
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

