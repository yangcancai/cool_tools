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
%%% Created : 2021-09-08T09:27:16+00:00
%%%-------------------------------------------------------------------
-module(cool_tools_metrics).

-author("yangcancai").

-export([ensure_metric/3, update/3]).

-spec update(Host :: jid:lserver() | global, Name :: term() | list(), Change :: term()) ->
                any().
update(Host, Name, Change) when is_list(Name) ->
    exometer:update([Host, Name], Change);
update(Host, Name, Change) ->
    update(Host, [Name], Change).

-spec ensure_metric(jid:lserver() | global, atom() | list(), term()) ->
                       ok | {ok, already_present} | {error, any()}.
ensure_metric(Host, Metric, Type) when is_tuple(Type) ->
    ensure_metric(Host, Metric, Type, element(1, Type));
ensure_metric(Host, Metric, Type) ->
    ensure_metric(Host, Metric, Type, Type).

ensure_metric(Host, Metric, Type, ShortType) when is_atom(Metric) ->
    ensure_metric(Host, [Metric], Type, ShortType);
ensure_metric(Host, Metric, Type, probe = ShortType) ->
    PrefixedMetric = [Host, Metric],
    {ShortType, Opts} = Type,
    case exometer:info(PrefixedMetric, type) of
        undefined ->
            ExometerOpts = [{type, ShortType}] ++ Opts,
            do_create_metric(PrefixedMetric, ad_hoc, ExometerOpts);
        _ ->
            {ok, already_present}
    end;
ensure_metric(Host, Metric, Type, ShortType) when is_list(Metric) ->
    %% the split into ShortType and Type is needed because function metrics are
    %% defined as tuples (that is Type), while exometer:info returns only 'function'
    PrefixedMetric = [Host, Metric],
    case exometer:info(PrefixedMetric, type) of
        undefined ->
            do_create_metric(PrefixedMetric, Type, []);
        ShortType ->
            {ok, already_present}
    end.

do_create_metric(PrefixedMetric, ExometerType, ExometerOpts) ->
    case catch exometer:new(PrefixedMetric, ExometerType, ExometerOpts) of
        {'EXIT', {exists, _}} ->
            {ok, already_present};
        ok ->
            ok;
        {'EXIT', Error} ->
            {error, Error}
    end.
