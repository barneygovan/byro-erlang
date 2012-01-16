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
		 get/2]).


%%
%% API Functions
%%
read_config_file(Filename, filename) ->
	{ok, File} = file:open(Filename, read),
	read_config_file(File, file);
read_config_file(File, file) ->
	ets:new(?MODULE, [named_table, set, protected]),
	read_line(File, none).

get(SectionName, Key) ->
	case ets:lookup(?MODULE, {SectionName, Key}) of
		[] ->
			none;
		[{_,Value}] ->
			Value
	end.

%%
%% Local Functions
%%
read_line(File, CurrentSection) ->
	case io:get_line(File) of 
		eof ->
			file:close(File);
		Line ->
			case process_line(Line) of
				comment -> 
					read_line(File, CurrentSection);
				{section, SectionName} ->
					read_line(File, SectionName);
				{ok, Key, Value} ->
					ets:insert({CurrentSection, Key}, Value),
					read_line(File, CurrentSection);
				bad_config_line ->
					bds_event:log_error("Badly formatted config line: " ++ Line),
					read_line(File, CurrentSection)
			end
	end.

process_line(Line) ->
	case string:strip(Line) of 
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
			
		
	
