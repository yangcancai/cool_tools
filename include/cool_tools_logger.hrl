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
%%% Created : 2021-11-18T04:20:04+00:00
%%%-------------------------------------------------------------------

-author("yangcancai").

-ifndef(H_cool_tools_log).

-define(H_cool_tools_log, true).

-include_lib("kernel/include/logger.hrl").

-define(LOG_IF(Level, Condition, Msg), Condition == true andalso ?LOG(Level, Msg)).
-define(LOG_IF(Level, Condition, Format, Args),
        Condition == true andalso ?LOG(Level, Format, Args)).
-define(DEPRECATE(Block),
        begin
            % cool_tools_logger:deprecated_logging(?LOCATION),
            Block
        end).
-define(DEBUG(A), ?DEPRECATE(?LOG_DEBUG(A))).
-define(DEBUG(Format, Args), ?DEPRECATE(?LOG_DEBUG(Format, Args))).
-define(DEBUG_IF(Condition, Format, Args),
        ?DEPRECATE(?LOG_IF(debug, Condition, Format, Args))).
-define(INFO_MSG(A), ?DEPRECATE(?LOG_INFO(A))).
-define(INFO_MSG(Format, Args), ?DEPRECATE(?LOG_INFO(Format, Args))).
-define(INFO_MSG_IF(Condition, A), ?DEPRECATE(?LOG_IF(info, Condition, A))).
-define(INFO_MSG_IF(Condition, Format, Args),
        ?DEPRECATE(?LOG_IF(info, Condition, Format, Args))).
-define(WARNING_MSG(A), ?DEPRECATE(?LOG_WARNING(A))).
-define(WARNING_MSG(Format, Args), ?DEPRECATE(?LOG_WARNING(Format, Args))).
-define(WARNING_MSG_IF(Condition, A), ?DEPRECATE(?LOG_IF(warning, Condition, A))).
-define(WARNING_MSG_IF(Condition, Format, Args),
        ?DEPRECATE(?LOG_IF(warning, Condition, Format, Args))).
-define(ERROR_MSG(A), ?DEPRECATE(?LOG_ERROR(A))).
-define(ERROR_MSG(Format, Args), ?DEPRECATE(?LOG_ERROR(Format, Args))).
-define(ERROR_MSG_IF(Condition, A), ?DEPRECATE(?LOG_IF(error, Condition, A))).
-define(ERROR_MSG_IF(Condition, Format, Args),
        ?DEPRECATE(?LOG_IF(error, Condition, Format, Args))).
-define(CRITICAL_MSG(A), ?DEPRECATE(?LOG_CRITICAL(A))).
-define(CRITICAL_MSG(Format, Args), ?DEPRECATE(?LOG_CRITICAL(Format, Args))).
-define(CRITICAL_MSG_IF(Condition, A), ?DEPRECATE(?LOG_IF(critical, Condition, A))).
-define(CRITICAL_MSG_IF(Condition, Format, Args),
        ?DEPRECATE(?LOG_IF(critical, Condition, Format, Args))).
-define(UNEXPECTED_INFO(Msg), ?LOG_WARNING(#{what => unexpected_info, msg => Msg})).
-define(UNEXPECTED_CAST(Msg), ?LOG_WARNING(#{what => unexpected_cast, msg => Msg})).
-define(UNEXPECTED_CALL(Msg, From),
        ?LOG_WARNING(#{what => unexpected_call,
                       msg => Msg,
                       call_from => From})).
-define(TRY_CATCH(Block, What),
        begin
            M = try
                    Block
                catch
                    E:R:Stack ->
                        #{what => What,
                          error => E,
                          reason => R,
                          stacktrace => Stack}
                end,
            ?ERROR_MSG(M)
        end).

-endif.
