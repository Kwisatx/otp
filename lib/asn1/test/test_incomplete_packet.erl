%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
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
-module(test_incomplete_packet).

-export([test/2]).

-include_lib("test_server/include/test_server.hrl").


%% testing OTP-5104

test(Opt, Config) ->
    Msg1 = {'PersonnelRecord',{'Name',"Sven","S","Svensson"},
           "manager",123,"20000202",{'Name',"Inga","K","Svensson"},
           []},
    test('P-Record','PersonnelRecord',Msg1,Opt,Config)   ,
    ok.

test(Module, Type, Value, Opt, _Config) ->
    {ok,UnSplit} = asn1_wrapper:encode(Module,Type,Value),
    {Split1,_Split2} = if is_binary(UnSplit) ->
                              split_binary(UnSplit, 8);
                          is_list(UnSplit) ->
                              {lists:sublist(UnSplit,8),lists:nthtail(8,UnSplit)}
                       end,
    case Opt of
        undec_rest ->
            {error,incomplete} = (catch asn1_wrapper:decode(Module,Type,Split1)),
            {ok,Value,<<>>}    = asn1_wrapper:decode(Module,Type,UnSplit);
        _ ->
            {error,incomplete} = (catch asn1_wrapper:decode(Module,Type,Split1)),
            {ok,Value}         = asn1_wrapper:decode(Module,Type,UnSplit)
    end.
