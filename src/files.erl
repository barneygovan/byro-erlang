-module(files).
-include_lib("kernel/include/file.hrl").

-export([get_file_list/1, get_file_list/2, get_added_files/2, get_deleted_files/2, get_filtered_file_list/2, get_filtered_file_list/3]).

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

walk_file_tree(Files, L) ->
    walk_file_tree(Files, L, []).

walk_file_tree([], L, _) -> 
    L;
walk_file_tree([File|Rest], L, IgnoreList) when is_list(File) ->
    case is_symlink(File) of 
        true ->
            walk_file_tree(Rest, L);
        false -> 
            case filelib:is_dir(File) of
                true -> 
                    {ok, Children} = file:list_dir(File),
                    ChildrenPaths = lists:map(add_this_path(File), filter_files(IgnoreList,Children,File)),
                    DirList = walk_file_tree(ChildrenPaths, L, IgnoreList),
                    walk_file_tree(Rest, DirList, IgnoreList);
                false -> 
                    {Result, Data} = signature(File, sha1),
                    case Result of 
                        ok ->
                            {FileNameSignature, FileSignature} = Data,
                            walk_file_tree(Rest, [{FileNameSignature, FileSignature, File, last_write_time(File)}|L], IgnoreList);
                        _ -> walk_file_tree(Rest, L, IgnoreList)
                    end
             end
    end.

get_file_list(File,IgnoreList) when is_list(IgnoreList) ->
    Sigs = walk_file_tree([File],[],IgnoreList),
    lists:sort(fun({X,_,_,{{_,_,_},{_,_,_}}},{Y,_,_,{{_,_,_},{_,_,_}}}) -> X < Y end, Sigs);
get_file_list(File,PathType) when is_atom(PathType) ->
    get_file_list(File,PathType,[]).

get_file_list(File,relative,IgnoreList) ->
    case filelib:is_dir(File) of
        true ->
            {_Result, Cwd} = file:get_cwd(),
            shell_default:cd(File),
            FileList = get_file_list(".",IgnoreList),
            shell_default:cd(Cwd),
            FileList;
        false ->
            {error, "You must supply a path to a root directory"}
    end;
get_file_list(File,absolute,IgnoreList) ->
    get_file_list(File,IgnoreList).


get_file_list(File) ->
    get_file_list(File, relative).

get_filtered_file_list(File,IgnoreList) ->
    get_filtered_file_list(File,IgnoreList,relative).

get_filtered_file_list(File,IgnoreList,PathType) ->
    get_file_list(File,PathType,IgnoreList).

filter_files([], L, _) ->
    L;
filter_files([CurrentPattern|Rest], L, Cwd) ->
    Excluded = filelib:wildcard(CurrentPattern, Cwd),
    filter_files(Rest, lists:filter(fun(X) -> lists:member(X, Excluded) =:= false end, L), Cwd).
    

add_this_path(".") ->
    (fun(Filename) -> Filename end);
add_this_path(Path) ->
    (fun(Filename) -> Path ++ "/" ++ Filename end).

last_write_time(Filename) ->
    {Result, FileInfo} = file:read_file_info(Filename),
    case Result of
        ok -> 
            FileInfo#file_info.mtime;
        Error -> Error 
    end.

get_added_files(OldFileList,File) ->
    OldFileSet = sets:from_list(OldFileList),
    NewFileList = get_file_list(File),
    NewFileSet = sets:from_list(NewFileList),
    sets:to_list(sets:subtract(NewFileSet, OldFileSet)).

get_deleted_files(OldFileList,File) ->
    OldFileSet = sets:from_list(OldFileList),
    NewFileList = get_file_list(File),
    NewFileSet = sets:from_list(NewFileList),
    sets:to_list(sets:subtract(OldFileSet, NewFileSet)).

