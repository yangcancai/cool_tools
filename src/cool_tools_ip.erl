%%%-------------------------------------------------------------------
%%% @author yangcancai

%%% Copyright (c) 2021 by yangcancai(yangcancai0112@gmail.com), All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       https://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

%%% @doc
%%%
%%% @end
%%% Created : 2021-12-30T09:53:34+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_ip).

-author("yangcancai").

-export([ip_to_int/1, int_to_ip/1]).
-export([ip_between/3]).

%% @doc Converts a binary string with a human readable ip
%% address representation into an uint32.
-spec ip_to_int(binary()) -> pos_integer().
ip_to_int(Ip) ->
    [O1Bin, O2Bin, O3Bin, O4Bin] = binary:split(Ip, <<".">>, [global]),
    B1 = binary_to_integer(O1Bin) bsl 24,
    B2 = binary_to_integer(O2Bin) bsl 16,
    B3 = binary_to_integer(O3Bin) bsl 8,
    B4 = binary_to_integer(O4Bin),
    B1 + B2 + B3 + B4.

%% @doc Converts the given uint32 into a binary string with the
%% human-readable ip address representation, i.e: <<"x.x.x.x">>.
-spec int_to_ip(pos_integer()) -> binary().
int_to_ip(Num) ->
    B1 = Num band 2#11111111000000000000000000000000 bsr 24,
    B2 = Num band 2#00000000111111110000000000000000 bsr 16,
    B3 = Num band 2#00000000000000001111111100000000 bsr 8,
    B4 = Num band 2#00000000000000000000000011111111,
    <<(integer_to_binary(B1))/binary,
      ".",
      (integer_to_binary(B2))/binary,
      ".",
      (integer_to_binary(B3))/binary,
      ".",
      (integer_to_binary(B4))/binary>>.

%% @doc Checks if the given IP address falls into the given network
%% range. E.g: ip_between(<<"192.168.0.1">>, <<"192.168.0.0">>, 16).
-spec ip_between(binary(), binary(), pos_integer()) -> boolean().
ip_between(Ip, Network, NetworkBits) ->
    IpNum = ip_to_int(Ip),
    NetLow = ip_to_int(Network),
    BitsHosts = 32 - NetworkBits,
    NetHigh =
        NetLow
        + erlang:trunc(
              math:pow(2, BitsHosts))
        - 1,
    IpNum >= NetLow andalso IpNum =< NetHigh.
