%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(testChoRecursive).


-export([recursive/1]).

-include_lib("test_server/include/test_server.hrl").

-record('ChoRec_something',{a, b, c}).
-record('ChoRec2_something',{a, b, c}).

recursive(_Rules) ->
    roundtrip('ChoRec',
	      {something,
	       #'ChoRec_something'{a = 77,
				   b = "some octets here",
				   c = {nothing,'NULL'}}}),
    roundtrip('ChoRec', {nothing,'NULL'}),
    roundtrip('ChoRec2',
	      {something,
	       #'ChoRec2_something'{a = 77,
				    b = "some octets here",
				    c = {nothing,'NULL'}}}),
    roundtrip('ChoRec2', {nothing,'NULL'}),
    ok.

roundtrip(Type, Value) ->
    {ok,Encoded} = 'ChoRecursive':encode(Type, Value),
    {ok,Value} = 'ChoRecursive':decode(Type, Encoded),
    case byte_size(Encoded) of
        1 -> ok;
        2 -> {error,incomplete} = 'ChoRecursive':decode(Type, element(1, split_binary(Encoded, 1)));
        3 -> {error,incomplete} = 'ChoRecursive':decode(Type, element(1, split_binary(Encoded, 1))),
             {error,incomplete} = 'ChoRecursive':decode(Type, element(1, split_binary(Encoded, 2)));
        N -> {error,incomplete} = 'ChoRecursive':decode(Type, element(1, split_binary(Encoded, N-1))),
             {error,incomplete} = 'ChoRecursive':decode(Type, element(1, split_binary(Encoded, N-2))),
             {error,incomplete} = 'ChoRecursive':decode(Type, element(1, split_binary(Encoded, N-3)))
    end,
    ok.
