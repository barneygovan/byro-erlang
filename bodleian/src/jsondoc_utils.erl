%% Author: barney
%% Created: Nov 19, 2011
%% Description: TODO: Add description to jsondoc_utils
-module(jsondoc_utils).

-record(jsondoc, {'_id', '_ver', type, body}).
-record(couchdb_response, {total_rows, offset, rows}).
-record(manifestdoc, {id, key, value}).
-record(create_response, {ok, id, rev}).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([add_header/3,
         strip_header/1,
         get_docs_from_couchdb_response/1,
         create_user_views/0,
         process_manifest_list/2,
         get_file_id/1]).

%%
%% API Functions
%%
add_header(Id, Type, Body) ->
    case Id of
        [] ->
            Doc = #jsondoc{type=Type, body=Body};
        _ ->
            Doc = #jsondoc{'_id'=Id, type=Type, body=Body}
    end,
    rfc4627:from_record(Doc, jsondoc, record_info(fields, jsondoc)).

strip_header(Doc) ->
    JsonRecord = rfc4627:to_record(Doc, #jsondoc{}, record_info(fields, jsondoc)),
    JsonRecord#jsondoc.body.

get_docs_from_couchdb_response(Doc) ->
    CouchDbResponse = rfc4627:to_record(Doc, #couchdb_response{}, record_info(fields, couchdb_response)),
    CouchDbResponse#couchdb_response.rows.
%   {ok, DecodedBody, _Raw} = rfc4627:decode(Doc).

create_user_views() ->
    ManifestView = {obj, [{map, <<"function(doc) {\n  if(doc.type == \"manifest\")\n  emit(doc._id, null);\n}">>}]},
    Views = {obj, [{get_manifest_list, ManifestView}]},          
    rfc4627:encode({obj, [{language, <<"javascript">>},
                          {views, Views}]}).

process_manifest_list([], ManifestList) ->
    lists:reverse(ManifestList);
process_manifest_list([Manifest|Tail], ManifestList) ->
    ManifestRecord = rfc4627:to_record(Manifest, #manifestdoc{}, record_info(fields, manifestdoc)),
    process_manifest_list(Tail, [ManifestRecord#manifestdoc.id|ManifestList]).

get_file_id(FileResponse) ->
    ResponseRecord = rfc4627:to_record(FileResponse, #create_response{}, record_info(fields, create_response)),
    ResponseRecord#create_response.id.

%%
%% Local Functions
%%

