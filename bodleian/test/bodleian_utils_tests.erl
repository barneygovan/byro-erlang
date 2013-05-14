%% Author: barney
%% Created: Feb 14, 2012
%% Description: TODO: Add description to bodleian_utils_tests
-module(bodleian_utils_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
compare_versions_equal_test() ->
	Version1 = "1.2.3",
	Version2 = "1.2.3",
	?assertEqual(true, bodleian_utils:compare_versions(Version1, Version2, equal)),
	
	Version3 = "1.2.0",
	Version4 = "1.2",
	?assertEqual(true, bodleian_utils:compare_versions(Version3, Version4, equal)),
	?assertEqual(true, bodleian_utils:compare_versions(Version4, Version3, equal)).


compare_versions_notequal_test() ->
	Version1 = "1.2.0",
	Version2 = "1.2.3",
	?assertEqual(false, bodleian_utils:compare_versions(Version1, Version2, equal)),
	
	Version3 = "1.2.3",
	Version4 = "1.2",
	?assertEqual(false, bodleian_utils:compare_versions(Version3, Version4, equal)).
	
compare_versions_min_less_test() ->
	Version1 = "1.2.3",
	Version2 = "1.2.0",
	?assertEqual(false, bodleian_utils:compare_versions(Version1, Version2, minimum)),

	Version3 = "1.2.1",
	Version4 = "1.2",
	?assertEqual(false, bodleian_utils:compare_versions(Version3, Version4, minimum)).

compare_versions_min_greater_test() ->
	Version1 = "1.2.3",
	Version2 = "1.2.4",
	?assertEqual(true, bodleian_utils:compare_versions(Version1, Version2, minimum)),
	
	Version3 = "1.2.3",
	Version4 = "1.3.3",
	?assertEqual(true, bodleian_utils:compare_versions(Version3, Version4, minimum)),
	
	Version5 = "1.2.3",
	Version6 = "1.2.3",
	?assertEqual(true, bodleian_utils:compare_versions(Version5, Version6, minimum)),

	Version7 = "1.2",
	Version8 = "1.2.3",
	?assertEqual(true, bodleian_utils:compare_versions(Version7, Version8, minimum)),

	Version9 = "1.2",
	Version10 = "1.2.0",
	?assertEqual(true, bodleian_utils:compare_versions(Version9, Version10, minimum)).

compare_versions_min_major_less_test() ->
	Version1 = "2.3.4",
	Version2 = "1.3.4",
	?assertEqual(false, bodleian_utils:compare_versions(Version1, Version2, min_major)),
 
	Version3 = "2",
	Version4 = "1.2.3",
	?assertEqual(false, bodleian_utils:compare_versions(Version3, Version4, min_major)).
    
compare_versions_min_major_greater_test() ->
	Version1 = "2.3.4",
	Version2 = "2.2.1",
	?assertEqual(true, bodleian_utils:compare_versions(Version1, Version2, min_major)),
	
	Version3 = "2.3.4",
	Version4 = "2.3.5",
	?assertEqual(true, bodleian_utils:compare_versions(Version3, Version4, min_major)),
	
	Version5 = "2.3.4",
	Version6 = "2.3.4",
	?assertEqual(true, bodleian_utils:compare_versions(Version5, Version6, min_major)),

    Version7 = "1.2.3",
	Version8 = "1",
	?assertEqual(true, bodleian_utils:compare_versions(Version7, Version8, min_major)).

%%
%% Local Functions
%%

