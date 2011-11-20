-module(user_default).

-export([la/0]).

la() -> 
    Modules = [M || {M, P} <- code:all_loaded(), is_list(P) andalso string:str(P, "fs/ebin") > 0],
    [shell_default:l(M) || M <- Modules].

