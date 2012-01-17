%% Author: barney
%% Created: Dec 28, 2011
%% Description: TODO: Add description to bodleian_config
-module(bodleian_config).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([read_config_file/2,
         read_config_file/3,
		 get/2,
         get/3]).

%%
%% Macros
%%
-define(TABLE_NAME, ?MODULE).

%%
%% API Functions
%%
read_config_file(Filename, filename, TableName) ->
    case file:read_file(Filename) of 
        {ok, FileContents} ->
            read_config_file(binary_to_list(FileContents), text, TableName);
        {error, enoent} ->
            file_not_found
    end;
read_config_file(Text, text, TableName) ->
    case Text of 
        "" ->
            {error, empty_file};
        _ ->
            Lines = string:tokens(Text, "\r\n"),
	        ets:new(TableName, [named_table, set, protected]),
            read_line(Lines, none, TableName)
    end.

read_config_file(Filename, filename) ->
    read_config_file(Filename, filename, ?TABLE_NAME);
read_config_file(Lines, lines) ->
    read_config_file(Lines, lines, ?TABLE_NAME).

get(SectionName, Key, TableName) ->
	case ets:lookup(TableName, {SectionName, Key}) of
		[] ->
			none;
		[{_,Value}] ->
			Value
	end.

get(SectionName, Key) ->
    get(SectionName, Key, ?TABLE_NAME).

%%
%% Local Functions
%%
read_line([], _CurrentSection, _TableName) ->
    ok;
read_line([Line|Lines], CurrentSection, TableName) ->
    case process_line(Line) of
        comment ->
            read_line(Lines, CurrentSection, TableName);
        empty ->
            read_line(Lines, CurrentSection, TableName);
        {section, SectionName} ->
            read_line(Lines, SectionName, TableName);
        {ok, Key, Value} ->
            ets:insert(TableName, {{CurrentSection, Key}, Value}),
            read_line(Lines, CurrentSection, TableName);
        bad_config_line ->
            bds_event:log_error("Badly formatted config line: " ++ Line),
            read_line(Lines, CurrentSection, TableName)
    end.

process_line(Line) ->
	case string:strip(Line) of 
        "" ->
            empty;
		%% Handle the new category
		"[" ++ Rest ->
			[NewSectionName|_Other] = string:tokens(Rest, "]"),
			{section, NewSectionName};
		%% Handle Comments
		";" ++ _Rest ->
			comment;
		ConfigLine ->
			Tokens = string:tokens(ConfigLine, "="),
			case length(Tokens) of
				2 ->
					[Key, ValueString] = Tokens,
					%% Remove any comments from the end of the line
					[Value|_Other] = string:tokens(ValueString, ";"),
					{ok, string:strip(Key), string:strip(Value)};
				_AnyOtherValue ->
					%% this is not a valid config line
					bad_config_line
			end
	end.
			
		
	
