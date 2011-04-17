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
    walk_file_tree(Files, L, nil, []).

walk_file_tree([], L, _, _) -> 
    L;
walk_file_tree([File|Rest], L, FilterFun, IgnoreList) when is_list(File) ->
    case is_symlink(File) of 
        true ->
            walk_file_tree(Rest, L);
        false -> 
            case filelib:is_dir(File) of
                true -> 
                    {ok, Children} = file:list_dir(File),
                    case FilterFun == nil of
                        false ->
                            ChildrenPaths = lists:map(add_this_path(File), filter_files(FilterFun,IgnoreList,Children));
                        true ->
                            ChildrenPaths = lists:map(add_this_path(File), Children)
                        end,
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

get_file_list(File,FilterFun,IgnoreList) ->
    Sigs = walk_file_tree([File],[],FilterFun,IgnoreList),
    lists:sort(fun({X,_,_,{{_,_,_},{_,_,_}}},{Y,_,_,{{_,_,_},{_,_,_}}}) -> X < Y end, Sigs).

get_file_list(File,relative,FilterFun,IgnoreList) ->
    case filelib:is_dir(File) of
        true ->
            shell_default:cd(File),
            get_file_list(".", FilterFun,IgnoreList);
        false ->
            {error, "You must supply a path to a root directory"}
    end;
get_file_list(File,absolute,FilterFun,IgnoreList) ->
    get_file_list(File,FilterFun,IgnoreList).

get_file_list(File,PathType)->
    get_file_list(File,PathType,nil,[]).

get_file_list(File) ->
    get_file_list(File, relative).

get_filtered_file_list(File,IgnoreList) ->
    get_filtered_file_list(File,IgnoreList,relative).

get_filtered_file_list(File,IgnoreList,PathType) ->
    DontMatchMe = fun(Pat) -> (fun(X) -> case re:run(X, Pat) of {match,_}-> false; nomatch->true end end) end,
    get_file_list(File,PathType,DontMatchMe,IgnoreList).

filter_files(_, [], L) ->
    lists:reverse(L);
filter_files(FilterFun, [CurrentPattern|Rest], L) ->
    {Result, Regexp} = re:compile(CurrentPattern),
    case Result of
        ok ->
            filter_files(FilterFun, Rest, lists:filter(FilterFun(Regexp), L));
        Error -> Error
    end.
    

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

