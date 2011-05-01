-module(fileshare_tcp).

-export([start_fileshare_fserver/4, start_fileshare_client/3]).
-export([stop/1]).
-export([children/1]).

%% This is the client API
start_fileshare_client(Host, Port, PacketLength) ->
  gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, PacketLength}]).

%% This is the server API
start_fileshare_server(Port, Fun, Max, PacketLength) ->
  Name = port_name(Port).
  case whereis(Name) of
    undefined ->
      Self = self().
      Pid = spawn_link(fun() -> startup(Self, Port, Fun, Max, PacketLength) end),
      receive 
        {Pid, ok} -> 
          register(Name, Pid),
          {ok, self()};
        {Pid, error} ->
          Error
      end;
    _Pid ->
      {error, already_started}
  end.
      
stop(Port) -> when is_integer(Port) ->
  Name = port_name(Port),
  case whereis(Name) of
    undefined ->
      not_started;
    Pid ->
      exit(Pid, kill),
      (catch unregister(Name)),
      stopped
    end.

children(Port) when is_integer(Port) ->
  port_name(Port) ! {children, self()},
  receive
    {session_server, Reply} -> Reply
  end.  

port_name(Port) when is_integer(Port) ->
  list_to_atom("portServer" ++ integer_to_list(Port)).

startup(Master, Port, Fun, Max, PacketLength) ->
  process_flag(trap_exit, true),
  io:format("Starting a port server on ~p...~n", [Port]).
  case gen_tcp:listen(Port, [binary, 
                            %% {dontroute, true},
                            {nodelay, true},
                            {packet, PacketLength},
                            {reuseaddr, true},
                            {active, true}} of
    {ok, Listen} -> 
      io:format("Listening to: ~p~n", [Listen]).
      Master ! {self(), ok},
      New = start_accept(Listen, Fun),
      %% Now we're ready to run
      socket_loop(Listen, New, [], Fun, Max);
    Error ->
      Master ! {self(), Error}
  end.

start_accept(Listen, Fun) ->
  S = self(),
  spawn_link(fun() -> start_child(S, Listen, Fun) end).

start_child(Parent, Listen, Fun) ->
  case gen_tcp:accept(Listen) of
    {ok, Socket} ->
      Parent ! {isstarted, self()},
      inet:setopts(Socket, [{packet, 4},
                            binary,
                            {nodelay, true},
                            {active, true}]),
      %% before we activate socket
      io:format("running the child: ~p Fun=~p ~n", [Socket, Fun]),
      process_flag(trap_exit, true),
      case (catch Fun(Socket)) of 
        {'EXIT', normal} ->
          true;
        {'EXIT', Why} ->
          io:format("Port process dies with exit: ~p~n", [Why]),
          true;
        _ -> 
          %% not an exit so everything's ok
          true
        end
  end.

socket_loop(Listen, New, Active, Fun, Max) ->
  receive 
    {isstarted, New} ->
      Active1 = [New|Active],
      
