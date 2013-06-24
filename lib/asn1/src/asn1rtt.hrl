%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

%% Check if the binary is of the expected size when splitting.
%% Exit with an incomplete error if the binary is smaller than expected.
-define(check_split(Bin,Size),
    case Bin of
        <<A:Size/binary, B/binary>> ->
            {A,B};
        _ when is_binary(Bin), is_integer(Size) ->
            throw({error, incomplete});
        _ -> erlang:error(badarg)
    end).
-define(check_split(Bin,Size,Type1),
    case Bin of
        <<A:Size/Type1, B/binary>> ->
            {A,B};
        _ when is_binary(Bin), is_integer(Size) ->
            throw({error, incomplete});
        _ -> erlang:error(badarg)
    end).
-define(check_split(Bin,Size,Type1,Type2),
    case Bin of
        <<A:Size/Type1, B/Type2>> ->
            {A,B};
        _ when is_binary(Bin), is_integer(Size) ->
            throw({error, incomplete});
        _ -> erlang:error(badarg)
    end).
-define(check_split(Bin,Size,Type1,A,B),
    case Bin of
        <<A:Size/Type1, B/binary>> ->
            {A,B};
        _ when is_binary(Bin), is_integer(Size) ->
            throw({error, incomplete});
        _ -> erlang:error(badarg)
    end).

%% Check if the bitstring is of the expected size when splitting.
%% Exit with an incomplete error if the bitstring is smaller than expected.
-define(check_bitstring_split(Bin,Size),
    case Bin of 
        <<A:Size, B/bitstring>> ->
            {A,B};
        _ when is_bitstring(Bin), is_integer(Size) ->
            throw({error, incomplete});
        _ ->
            erlang:error(badarg)
    end).
-define(check_bitstring_split(Bin,Size,Type1),
    case Bin of
        <<A:Size/Type1, B/bitstring>> -> 
            {A,B};
        _ when is_bitstring(Bin), is_integer(Size) ->
            throw({error, incomplete});
        _ ->
            erlang:error(badarg)
    end).
-define(check_bitstring_split(Bin,Size,Type1,Type2),
    case Bin of
        <<A:Size/Type1, B/Type2>> ->
            {A,B};
        _ when is_bitstring(Bin), is_integer(Size) ->
            throw({error, incomplete});
        _ ->
            erlang:error(badarg)
    end).




