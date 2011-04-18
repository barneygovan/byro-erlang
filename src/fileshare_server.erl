-module(fileshare_server).
-behaviour(gen_server).

%% API calls
-export([list_files/2,start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

list_files(Root,IgnoreList) when is_list(Root),is_list(IgnoreList) ->
    gen_server:call(?MODULE, {listfiles, Root, IgnoreList}, 20000).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}.

handle_call({listfiles, Root, IgnoreList}, _From, N) ->
    {reply, files:get_filtered_file_list(Root, IgnoreList), N+1}.

handle_cast(_Msg, N) -> {noreply, N}.

handle_info(_Info, N) -> {noreply, N}.

terminate(_Reason, _N) -> 
    io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.








