-module(lib_sha1).
-export([string/1, file/1]).

-define(BLOCKSIZE, 32768).

string(Str) -> 
    crypto:start(),
    lib_md5:digest2str(crypto:sha(Str)).

file(File) ->
    crypto:start(),
    case file:open(File, [binary,raw,read]) of
        {ok, IoDevice} -> digestFromFile(IoDevice, crypto:sha_init());
        Error -> Error
    end.

digestFromFile(IoDevice, ShaContext) ->
    case file:read(IoDevice, ?BLOCKSIZE) of
        {ok, Data} -> digestFromFile(IoDevice, crypto:sha_update(ShaContext, Data));
        eof -> file:close(IoDevice),
               {ok, crypto:sha_final(ShaContext)}
    end.


