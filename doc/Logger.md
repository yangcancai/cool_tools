# Logger

To use logger in your module, include

```erlang
-include_lib("cool_tools/include/cool_tools_logger.hrl").
```
## sys.config

```erlang
[
{kernel, [
  %% Specifies the primary log level for Logger.
  %% Log events with the same, or a more severe level, pass through the primary log level check.
  %% This option is overwritten by MongooseIM config, once it is loaded.
  {logger_level, notice},

  {logger, [
    %% Default filters applied to all events before passing them to handlers:
    {filters, log, [
           %% If we want to see complete accumulator in logs
           %% {format_packet_filter, {fun M:format_packet_filter/2, no_state}},
           {format_stacktrace_filter, {fun cool_tools_logger:format_stacktrace_filter/2, no_state}}
           %% {format_term_filter, {fun M:format_term_filter/2, [toml_value]}}
        ]},

    %% Shell log handler:
    {handler, shell_log, logger_std_h, #{
         level => all,
         formatter => {flatlog, #{
           map_depth => 3,
           term_depth => 50
         }}
    }},

    %% There are two file handlers below, writing same log messages,
    %% but in different formats.
    %% Remove one of them, if you don't need it in production.
    %% Less handlers and less verbose log levels would improve performance.

    %% File log handler:
    {handler, disk_log, logger_disk_log_h, #{
         level => all,
         config => #{
           file => "log/cool_tools.log",
           type => wrap,
           max_no_files => 5,
           max_no_bytes => 2097152,
           sync_mode_qlen => 2000, % If sync_mode_qlen is set to the same value as drop_mode_qlen,
           drop_mode_qlen => 2000, % synchronous mode is disabled. That is, the handler always runs
           flush_qlen => 5000,     % in asynchronous mode, unless dropping or flushing is invoked.
           overload_kill_enable => true
           % Documentation about Overload protection, together with default values, can be found here:
           % http://erlang.org/doc/apps/kernel/logger_chapter.html#protecting-the-handler-from-overload
         },
         formatter => {flatlog, #{
           map_depth => 3,
           term_depth => 50
         }}
    }},

    %% JSON file log handler:
    {handler, disk_json_log, logger_disk_log_h, #{
         level => all,
         config => #{
           file => "log/cool_tools.json",
           type => wrap,
           max_no_files => 5,
           max_no_bytes => 2097152,
           sync_mode_qlen => 2000, % If sync_mode_qlen is set to the same value as drop_mode_qlen,
           drop_mode_qlen => 2000, % synchronous mode is disabled. That is, the handler always runs
           flush_qlen => 5000,     % in asynchronous mode, unless dropping or flushing is invoked.
           overload_kill_enable => true
           % Documentation about Overload protection, together with default values, can be found here:
           % http://erlang.org/doc/apps/kernel/logger_chapter.html#protecting-the-handler-from-overload
         },
         formatter => {cool_tools_json_formatter, #{
           format_depth => 10,
           format_chars_limit => 3000,
           depth => 10
         }}
    }}

  ]}]}
].
```
## Logging macros

There are several macros for the most common logging levels:

```erlang
?LOG_DEBUG(#{what => debug_event, info => Arg}),
?LOG_INFO(#{what => info_event, info => Arg}),
?LOG_NOTICE(#{what => notice_event, info => Arg}),
?LOG_WARNING(#{what => warning_event, info => Arg}),
?LOG_ERROR(#{what => error_event, info => Arg}),
?LOG_CRITICAL(#{what => critical_event, info => Arg}),
```

Use them in correspondence with the appropriate log level.
Please be mindful of what is logged and which log level is used for it.

## Logging levels

A system operator can choose the global log level by setting `loglevel` in `mongooseim.toml`.

Possible values are the standard syslog severity levels, plus all or none:
`"all"`, `"debug"`, `"info"`, `"notice"`, `"warning"`, `"error"`, `"critical"`, `"alert"`, `"emergency"`, and `"none"`.

```erlang
cool_tools_logger:set_global_loglevel(error).
```

If a user sets the log level to `all`, then they would see all messages in logs.

Levels `warning` and `error` are the most commonly used for production systems.

## Logging format

We use structured logging as inspired by [Ferd's post](https://ferd.ca/erlang-otp-21-s-new-logger.html).
We also use a modified [logfmt](https://brandur.org/logfmt) format as one of
the possible default logger formatters.
This format is [Splunk](https://www.splunk.com/en_us/devops.html) and [ELK](https://www.elastic.co/elk-stack) friendly.
Check [the list of fields](../operation-and-maintenance/Logging-fields.md) for fields documentation.

`what => something_interesting` field is required.

```erlang
    ?LOG_ERROR(#{what => check_password_failed,
                 reason => Error, user => LUser})

    try ...
    catch
        Class:Reason:StackTrace ->
            ?LOG_ERROR(#{what => check_password_failed,
                         class => Class, reason => Reason, stacktrace => StackTrace}),
            erlang:raise(Class, Reason, StackTrace)
    end
```

Field `user => <<"alice">>` is often used too.

A common way to name an error event is `what => function_name_failed`.
For example, `what => remove_user_failed`. Use the advice critically, it would
not work well for any function. Counterexample:

```erlang
handle_info(Info, State) ->
    ?LOG_WARNING(#{what => unexpected_message, msg => Info}),
    {noreply, State}.
```


## Filtering logs by module

Setting loglevel to `debug` can lead to a flood of messages in logs.
To set a different loglevel for just one module, call:

```erlang
cool_tools_logger:set_module_loglevel(cool_tools, debug).
```

This code sets the loglevel to error for all log messages, except for those generated by `cool_tools`.
All messages from `cool_tools` would be logged.

