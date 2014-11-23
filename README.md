rebar_escript_plugin
=========================

[![Build Status](https://travis-ci.org/schlagert/rebar_escript_plugin.png?branch=master)](https://travis-ci.org/schlagert/rebar_escript_plugin)

A `rebar3` plugin to package applications as executable escript.

Common problems with `rebar2 escriptize` addressed:
* the escript doesn't start because you forgot to provide a `main/1` function in
  the correct module
* the escript doesn't contain the dependencies because you forgot to duplicate
  the `deps` information into `escript_incl_apps`
* you can't access your `priv` data at runtime

This is where the `rebar3` `rebar_escript_plugin` tries to help.

Now tell me what's so _terribly_ different from `rebar2` `escriptize`? In fact not
much, the plugin
* avoids the necessity of duplicate configuration.
* provides a default main/1 function, starting all needed applications as a
  normal Erlang release would.
* makes your application's `priv` directory contents accessible at runtime.

Usage
-----

This branch of `rebar_escript_plugin` currently needs [rebar3](https://github.com/rebar/rebar3)
 and [Erlang/OTP](http://erlang.org) `17.0` or newer.

First of all the plugin must reside somewhere in your project's code path. This
can be achieved by either placing the application somewhere into your `lib_dir`
(or `ERL_LIBS`) or by specifying it as a project dependency in the `deps`
section of your project's `rebar.config`. Ultimatively, you also need to include
the plugin in your project's `rebar.config`. The most common configuration would
look something like this:

```erlang
{plugins, [{rebar_escript_plugin, "", {git, "https://github.com/tsloughter/rebar_escript_plugin.git", {branch, "master"}}}]}.
%% Only needed if your project has multiple apps or you depend on
%% special emulator args, e.g. Erlang distribution
{rebar_escript_plugin, [{main_app, module()},
                        {emu_args, string()}]}.
```

Now you can call `escriptize` from in the project:

```
$ rebar3 escriptize
===> Compiling app
===> Building escript
```
