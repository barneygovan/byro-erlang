%% Author: barney
%% Created: Nov 19, 2011
%% Description: TODO: Add description to jsondoc_utils
-module(jsondoc_utils).

-record(jsondoc, {'_id', '_ver', type, body}).
-record(couchdb_response, {total_rows, offset, rows}).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([add_header/3,
         strip_header/1,
         get_docs_from_couchdb_response/1,
         create_user_views/0]).

%%
%% API Functions
%%
add_header(Id, Type, Body) ->
    Doc = #jsondoc{'_id'=Id, type=Type, body=Body},
    rfc4627:from_record(Doc, jsondoc, record_info(fields, jsondoc)).

strip_header(Doc) ->
    JsonRecord = rfc4627:to_record(Doc, #jsondoc{}, record_info(fields, jsondoc)),
    JsonRecord#jsondoc.body.

get_docs_from_couchdb_response(Doc) ->
    CouchDbResponse = rfc4627:to_record(Doc, #couchdb_response{}, record_info(fields, couchdb_response)),
    CouchDbResponse#couchdb_response.rows.
%   {ok, DecodedBody, _Raw} = rfc4627:decode(Doc).

create_user_views() ->
    ManifestView = {obj, [{map, <<"function(doc) {\n  if(doc.type == \"manifest\")\n  emit(null, doc);\n}">>}]},
    Views = {obj, [{get_manifest_list, ManifestView}]},          
    rfc4627:encode({obj, [{language, <<"javascript">>},
                          {views, Views}]}).

%%
%% Local Functions
%%

