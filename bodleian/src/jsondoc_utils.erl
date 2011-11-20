%% Author: barney
%% Created: Nov 19, 2011
%% Description: TODO: Add description to jsondoc_utils
-module(jsondoc_utils).

-record(jsondoc, {owner, id, type, body}).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([add_header/4,
         strip_header/1]).

%%
%% API Functions
%%
add_header(Owner, Id, Type, Body) ->
    Doc = #jsondoc{owner=Owner, id=Id, type=Type, body=Body},
    rfc4627:from_record(Doc, jsondoc, record_info(fields, jsondoc)).

strip_header(Doc) ->
    JsonRecord = rfc4627:to_record(Doc, #jsondoc{}, record_info(fields, jsondoc)),
    JsonRecord#jsondoc.body.


%%
%% Local Functions
%%

