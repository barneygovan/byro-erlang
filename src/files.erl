-module(files).
-include_lib("kernel/include/file.hrl").

-export([walk_file_tree/1, get_added_files/2]).

gen_file_signature(Filename, F) ->
    {SignatureResult, Data} = F(Filename),
    case SignatureResult of
        ok -> {ok, lib_md5:digest2str(Data)};
        _ -> {SignatureResult, Data}
    end.

signature(Filename, name_signature, md5) -> {ok, catch lib_md5:string(Filename)};
signature(Filename, name_signature, sha1) -> {ok, lib_sha1:string(Filename)};
signature(Filename, contents_signature, md5) -> gen_file_signature(Filename, fun lib_md5:file/1);
signature(Filename, contents_signature, sha1) -> gen_file_signature(Filename, fun lib_sha1:file/1);
signature(Filename, Kind, Algorithm) ->
    {error, {Filename, badargs, Kind, Algorithm}}.

signature(Filename, Algorithm) -> 
    {NameResult, NameSignature} = signature(Filename, name_signature, Algorithm),
    case NameResult of 
        ok -> 
            {ContentsResult, ContentsSignature} = signature(Filename, contents_signature, Algorithm),
            case ContentsResult of
                ok -> {ok, {NameSignature, ContentsSignature}};
                _ -> {error, ContentsSignature}
            end;
        _ -> {error, NameSignature}
    end.

is_symlink(Filename) ->
    case file:read_link(Filename) of
        {ok, _} -> true;
        _ -> false
    end.

walk_file_tree([], L) -> 
    L;
walk_file_tree([File|Rest], L) when is_list(File) ->
    case is_symlink(File) of 
        true ->
            walk_file_tree(Rest, L);
        false -> 
            case filelib:is_dir(File) of
                true -> 
                    {ok, Children} = file:list_dir(File),
                    ChildrenPaths = lists:map(add_this_path(File), Children),
                    DirList = walk_file_tree(ChildrenPaths, L),
                    walk_file_tree(Rest, DirList);
                false -> 
                    {Result, Data} = signature(File, sha1),
                    case Result of 
                        ok ->
                            {FileNameSignature, FileSignature} = Data,
                            walk_file_tree(Rest, [{FileNameSignature, FileSignature, File, last_write_time(File)}|L]);
                        _ -> walk_file_tree(Rest, L)
                    end
             end
    end.

walk_file_tree(File) ->
    Sigs = walk_file_tree([File], []),
    lists:sort(fun({X,_,_,{{_,_,_},{_,_,_}}},{Y,_,_,{{_,_,_},{_,_,_}}}) -> X < Y end, Sigs).

add_this_path(Path) ->
    (fun(Filename) -> Path ++ "/" ++ Filename end).

last_write_time(Filename) ->
    {Result, FileInfo} = file:read_file_info(Filename),
    case Result of
        ok -> 
            FileInfo#file_info.mtime;
        Error -> Error 
    end.

get_added_files(OldFileSet,File) ->
    NewFileList = walk_file_tree(File),
    NewFileSet = sets:from_list(NewFileList),
    sets:subtract(NewFileSet, OldFileSet).

%MyList = files:walk_file_tree("/home/barney/git/fs/src").
%MySet = sets:from_list(MyList).
%MyNewList = files:walk_file_tree("/home/barney/git/fs/src").
%MyNewSet = sets:from_list(MyNewList).
%DiffSet = sets:subtract(MyNewSet, MySet).
%DiffSet2 = sets:subtract(MySet, MyNewSet).
%sets:to_list(DiffSet2).
