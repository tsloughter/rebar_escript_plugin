%%%=============================================================================
%%% Copyright 2014, Tobias Schlager <schlagert@github.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%% A `rebar' plugin to package applications or releases as executable escript
%%% similar to `rebar escriptize' but without its limitations.
%%% To use it specify it in the `plugins' section of your project's `rebar.config', e.g.
%%% `{plugins, [{rebar_escript_plugin, "",
%%%               {git, "https://github.com/tsloughter/rebar_escript_plugin.git",
%%%                  {branch, "master"}}}]}.'
%%% @end
%%%=============================================================================
-module(rebar_escript_plugin).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, escriptize).
-define(DEPS, [compile]).

-define(TEMP_DIR, ".escript").
-define(MAIN_MODULE, rebar_escript_plugin_main).

-include_lib("kernel/include/file.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar escriptize"},
                                {opts, []},
                                {short_desc, ""},
                                {desc, ""}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_log:log(info, "Building escript", []),
    App1 = case rebar_state:project_apps(State) of
              [App] ->
                  App;
              Apps ->
                  Cfg = rebar_state:get(State, ?MODULE, []),
                  case proplists:get_value(main_app, Cfg, undefined) of
                      undefined ->
                          {error, {?MODULE, no_main_app}};
                      Name ->
                          rebar_app_utils:find(Name, Apps)
                  end
          end,

    run(State, App1),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(no_main_app) ->
    io_lib:format("Multiple project apps and {rebar_escript_plugin, [{main_app, atom()}]}. not set in rebar.config", []).

%%------------------------------------------------------------------------------
%% @doc
%% Create a standalone escript of an application and its dependencies in the
%% project's base directory after compilation with `rebar compile'.
%%
%% All applications located in `deps_dir' and `lib_dirs' will be included. If
%% not provided, the plugin will create a start module with a default `main/1'
%% function which uses {@link application:ensure_all_started/2} to bootstrap
%% the application.
%% @end
%%------------------------------------------------------------------------------
-spec run(rebar_state:t(), rebar_app_info:t()) -> ok.
run(State, App) ->
    BaseDir = rebar_state:get(State, base_dir, "."),
    TempDir = temp_dir(BaseDir),
    ok = filelib:ensure_dir(filename:join([TempDir, "dummy"])),
    ok = prepare_runner_module(TempDir),
    AppName = rebar_app_info:name(App),
    AppsToStart = [AppName],
    Deps = rebar_state:all_deps(State),
    Apps = [App | Deps],
    PackagedApps = mk_links(TempDir, Apps),
    ok = prepare_main_module(TempDir, AppsToStart, PackagedApps),
    Ez = create_ez(TempDir, App, PackagedApps),
    ok = create_escript(Ez, BaseDir, State, ec_cnv:to_list(AppName)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% Creates links to the application directories corresponding to the given list
%% of `.app' files in a temporary directory. This is needed to get the directory
%% structure desired by the code server for the created `.ez' archive.
%%------------------------------------------------------------------------------
mk_links(TempDir, Apps) ->
    [mk_link(TempDir, App) || App <- Apps].

%%------------------------------------------------------------------------------
%% @private
%% Creates a link to the application directory corresponding to the given
%% `.app' file.
%%------------------------------------------------------------------------------
mk_link(TempDir, App) ->
    LinkName = app_link(TempDir, App),
    case file:make_symlink(rebar_app_info:dir(App), LinkName) of
        ok ->
            rebar_app_info:name(App);
        {error, eexist} ->
            rebar_app_info:name(App);
        Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
app_link(TempDir, App) ->
    AppNameVsn = ec_cnv:to_list(rebar_app_info:name(App))
        ++ "-" ++ ec_cnv:to_list(rebar_app_info:original_vsn(App)),
    filename:join([TempDir, AppNameVsn]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
temp_dir(BaseDir) -> filename:join([BaseDir, ?TEMP_DIR]).

%%------------------------------------------------------------------------------
%% @private
%% Creates an `.ez' archive (in memory) to be embedded in the escript. The
%% archive will contain `ebin' and `priv' directories of all dependent
%% applications. The archive will have the following content layout:
%%
%% application-version.ez
%%  + rebar_escript_plugin_main.beam
%%  + rebar_escript_plugin_runner.beam
%%  + application-version/ebin
%%  + application-version/priv
%%  + dependency1-version/ebin
%%  + dependency1-version/priv
%%  + dependency2-version/ebin
%%  + dependency2-version/priv
%%  + ...
%%
%%------------------------------------------------------------------------------
create_ez(TempDir, App, PackagedApps) ->
    Apps = string:join([ec_cnv:to_list(A) || A <- PackagedApps], ","),
    Paths = filename:join(["{" ++ Apps ++ "}-*", "{ebin,priv}"]),
    AppNameVsn = ec_cnv:to_list(rebar_app_info:name(App))
        ++ "-" ++ ec_cnv:to_list(rebar_app_info:original_vsn(App)),
    {ok, {_, Archive}} =
        zip:create(
          AppNameVsn ++ ".ez",
          filelib:wildcard(Paths, TempDir) ++
              filelib:wildcard("*.beam", TempDir),
          [{cwd, TempDir}, {uncompress, all}, memory]),
    Archive.

%%------------------------------------------------------------------------------
%% @private
%% Returns the path to the escript to create. On windows systems the file will
%% have the extension `.escript', unix systems will omit the extension.
%%------------------------------------------------------------------------------
escript_path(OsFamily, BaseDir, EScript) ->
    Extension = escript_extension(OsFamily),
    filename:join([BaseDir, EScript ++ Extension]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
create_escript(Ez, BaseDir, State, EScript) ->
    {OsFamily, _OsName} = os:type(),
    Path = escript_path(OsFamily, BaseDir, EScript),
    HeartArg = "-heart",
    MainArg = "-escript main " ++ atom_to_list(?MAIN_MODULE),
    EmuArgs = string:join([HeartArg, MainArg, get_emu_args(State)], " "),
    Sections = [shebang, comment, {emu_args, EmuArgs}, {archive, Ez}],
    ok = escript:create(Path, Sections),
    ok = set_executable(OsFamily, Path).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
escript_extension(nt)      -> ".escript";
escript_extension(windows) -> ".escript";
escript_extension(_)       -> "".

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_executable(unix, EScript) ->
    {ok, Info} = file:read_file_info(EScript),
    NewMode = (((Info#file_info.mode bor 8#1) bor 8#10) bor 8#100),
    ok = file:write_file_info(EScript, Info#file_info{mode = NewMode});
set_executable(_, _EScript) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
main_path(TempDir, Module) ->
    filename:join([TempDir, atom_to_list(Module) ++ ".beam"]).

%%------------------------------------------------------------------------------
%% @private
%% Writes the code of the runner module into the temporary plugin directory.
%% This code will be needed at runtime to setup the correct environment.
%%------------------------------------------------------------------------------
prepare_runner_module(TempDir) ->
    Module = rebar_escript_plugin_runner,
    {Module, Binary, _} = code:get_object_code(Module),
    ok = file:write_file(main_path(TempDir, Module), Binary).

%%------------------------------------------------------------------------------
%% @private
%% Generates and writes the code of the main module into the temporary plugin
%% directory. This code provides the main entry point for escript execution (it
%% provides the `main/1' function).
%%------------------------------------------------------------------------------
prepare_main_module(TempDir, AppsToStart, PackagedApps) ->
    {ok, T1, _} = erl_scan:string(
                    "-module("
                    ++ atom_to_list(?MAIN_MODULE)
                    ++ ")."),
    {ok, T2, _} = erl_scan:string("-export([main/1])."),
    {ok, T3, _} = erl_scan:string(
                    "main(Args) ->"
                    "    rebar_escript_plugin_runner:main("
                    ++ lists:flatten(io_lib:format("~w", [AppsToStart]))
                    ++ ", Args, "
                    ++ lists:flatten(io_lib:format("~w", [PackagedApps]))
                    ++ ")."),
    {ok, F1} = erl_parse:parse_form(T1),
    {ok, F2} = erl_parse:parse_form(T2),
    {ok, F3} = erl_parse:parse_form(T3),
    {ok, ?MAIN_MODULE, Binary} = compile:forms([F1, F2, F3]),
    ok = file:write_file(main_path(TempDir, ?MAIN_MODULE), Binary).

%%------------------------------------------------------------------------------
%% @private
%% Returns a custom emulator argument string, if one. Please note that the
%% `-escript main' parameter is forbidden. It must point to the internal
%% generated module. Otherwise the execution environment will not be setup
%% correctly. Additionally, the `-heart' parameter is always set (for cleanup)
%% and it is therefore not necessary to specify it.
%%------------------------------------------------------------------------------
get_emu_args(State) ->
    Cfg = rebar_state:get(State, ?MODULE, []),
    case proplists:get_value(emu_args, Cfg, "") of
        EmuArgs when is_list(EmuArgs) -> EmuArgs
    end.
