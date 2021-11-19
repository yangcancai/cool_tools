cool_tools
----

![CI](https://github.com/yangcancai/cool_tools/actions/workflows/ci.yml/badge.svg)


功能列表
---------
* [Logger使用方法](./doc/Logger.md)
* [Backend中间模块生成](./doc/Backend.md)


Required
-----
	$ rebar3 -v
	rebar 3.14.4 on Erlang/OTP 22 Erts 10.7.2.1

Build
-----

    $ make co

Eunit
-----

    $ make eunit

Common Test
-----

    $ make ct

Dialyzer
----

    $ make dialyzer

Test(dialyzer, eunit, ct)
----

    $ make test

