%% Author: barney
%% Created: Feb 13, 2012
%% Description: TODO: Add description to bodleian_utils
-module(bodleian_utils).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([compare_versions/3]).

-define(VERSION_SEPARATOR, ".").
-define(ZERO_VERSION, "0").

%%
%% API Functions
%%
compare_versions(Version1, Version2, ComparisonType) ->
	Tokens1 = string:tokens(Version1, ?VERSION_SEPARATOR),
	Tokens2 = string:tokens(Version2, ?VERSION_SEPARATOR),
	compare_parts(Tokens1,Tokens2,ComparisonType).


%%
%% Local Functions
%%
compare_parts([],[], _ComparisonType) ->
	true;
compare_parts([Part1|_Parts1],[], ComparisonType) ->
	case ComparisonType of
		equal ->
			string:equal(Part1, ?ZERO_VERSION);
		minimum ->
			string:equal(Part1, ?ZERO_VERSION);
		_ ->
			{error,unknown_comparison}
	end;
compare_parts([],[Part2|_Parts2], ComparisonType) ->
	case ComparisonType of
		equal ->
			string:equal(Part2, ?ZERO_VERSION);
		minimum ->
			true;
		_ ->
			{error,unknown_comparison}
	end;
compare_parts([Part1|Parts1], [Part2|Parts2], ComparisonType) ->
	case ComparisonType of
		equal ->
			case string:equal(Part1, Part2) of
				true ->
					compare_parts(Parts1,Parts2,ComparisonType);
				false ->
					false
			end;
		minimum ->
			Int1 = string:to_integer(Part1),
			Int2 = string:to_integer(Part2),
			case Int1 =< Int2 of
				true ->
					compare_parts(Parts1,Parts2,ComparisonType);
				false ->
					false
			end;
		min_major ->
			Int1 = string:to_integer(Part1),
			Int2 = string:to_integer(Part2),
			Int1 =< Int2;
		_ ->
			{error,unknown_comparison}
	end.

