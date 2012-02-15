%% Author: barney
%% Created: Jan 1, 2012
%% Description: TODO: Add description to bodleian_config_test
-module(bodleian_config_tests).

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
read_config_file_simple_test() ->
    TableName = read_config_file_simple_test,
    Section1 = "Section1",
    Key1 = "Key1",
    Value1 = "Value1",
    Text = "[" ++ Section1 ++ "]\n" ++ Key1 ++ " = " ++ Value1 ++ "\n",
    ?assertEqual(ok, bodleian_config:read_config_file(Text, text, TableName)).
 
read_config_file_empty_test() ->
    TableName = read_config_file_empty_test,
    Text = "",
    ?assertEqual({error, empty_file}, bodleian_config:read_config_file(Text, text, TableName)).

read_config_file_with_comments_test() ->
    TableName = read_config_file_with_comments_test,
    Comment = ";This is a comment\n",
    Section1 = "Section1",
    Key1 = "Key1",
    Value1 = "Value1",
    Text = "[" ++ Section1 ++ "]\n" ++ Comment ++ Key1 ++ " = " ++ Value1 ++ "\n",
    ?assertEqual(ok, bodleian_config:read_config_file(Text, text, TableName)).

read_config_file_different_newlines_test() ->
    TableName = read_config_file_different_newlines_test,
    Comment = ";This is a comment\n",
    Section1 = "Section1",
    Key1 = "Key1",
    Value1 = "Value1",
    Text = "[" ++ Section1 ++ "]\r\n" ++ Comment ++ Key1 ++ " = " ++ Value1 ++ "\r",
    ?assertEqual(ok, bodleian_config:read_config_file(Text, text, TableName)).


get_simple_test() ->
    TableName = get_simple_test,
    Section1 = "Section1",
    Key1 = "Key1",
    Value1 = "Value1",
    Text = "[" ++ Section1 ++ "]\n" ++ Key1 ++ " = " ++ Value1 ++ "\n",
    ?assertEqual(ok, bodleian_config:read_config_file(Text, text, TableName)),
    TestValue = bodleian_config:get(Section1, Key1, TableName),
    ?assertEqual(TestValue, Value1).

get_complex_test() ->
    TableName = get_complex_test,
    Section1 = "Section1",
    Key1 = "Key1",
    Value1 = "Value1",
    Key2 = "Key2",
    Value2 = "Value2",
    
    Section2 = "Section2",
    Key3 = "Key3",
    Value3 = "Value3",
    
    Text = "[" ++ Section1 ++ "]\n" 
            ++ Key1 ++ " = " ++ Value1 ++ "\n"
            ++ Key2 ++ " =  " ++ Value2 ++ "\n"
            ++ "[" ++ Section2 ++ "]\n"
            ++ Key3 ++ "=" ++ Value3,
    ?assertEqual(ok, bodleian_config:read_config_file(Text, text, TableName)),
    TestValue1 = bodleian_config:get(Section1, Key1, TableName),
    ?assertEqual(TestValue1, Value1),
    TestValue2 = bodleian_config:get(Section1, Key2, TableName),
    ?assertEqual(TestValue2, Value2),
    TestValue3 = bodleian_config:get(Section2, Key3, TableName),
    ?assertEqual(TestValue3, Value3).
    

%%
%% Local Functions
%%

