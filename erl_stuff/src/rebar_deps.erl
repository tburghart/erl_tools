%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%
%%  @doc    The {@module} module provides operations for analyzing a rebar
%%          dependency graph.
%%
%%  Run with escript for ease of use, or the main functionality can be
%%  accessed through exported functions.
%%
-module(rebar_deps).

-include("rebar_deps.hrl").

%%======================================================================
%%  Public API
%%======================================================================
-export([
    main/1,
    write_csv/2,
    write_dot/2,
    write_txt/2,
    collect_confs/1,
    collect_deps/1,
    collect_files/2,
    abspath/1
]).

%%======================================================================
%%  Types
%%======================================================================

-type path()    :: file:filename().
-type label()   :: string().
-type plist()   :: [{label(), path()}].

-type option()  :: label | out | format | top.
-type options() :: #{option() => string()}.
-type name()    :: string().
-type url()     :: string().
-type repo()    :: {atom(), url()}.
-type regex()   :: string().
-type rev()     :: {atom(), string()} | string().
-type vrec()    :: {regex(), rev()}.
-type used()    :: #{vrec() => [name()]}.
-type basho()   :: boolean().

-type drec()     :: {basho(), [used()]}.

-type dmap()    :: #{name() => drec()}.
-type dlist()   :: [{name(), drec()}].

-type crec()    :: {name(), repo(), vrec()}.
-type conf()    :: {name(), [crec()]}.
-type clist()   :: [conf()].

%%======================================================================
%%  API functions
%%======================================================================

-spec main([string()]) -> ok | no_return().
%%  @doc    Escript main.
main(Args) ->
    Cfg   = parse_args(Args),
    Files = collect_files(maps:get(label, Cfg), maps:get(top, Cfg)),
    Confs = collect_confs(Files),
    Deps  = collect_deps(Confs),
    IoDev = case maps:get(out, Cfg, undefined) of
        undefined ->
            standard_io;
        OutFile ->
            {ok, FD} = file:open(OutFile, [write]),
            FD
    end,
    case maps:get(format, Cfg) of
        csv ->
            write_csv(IoDev, Deps);
        dot ->
            write_dot(IoDev, Deps);
        txt ->
            write_txt(IoDev, Deps)
    end,
    case is_atom(IoDev) of
        true ->
            ok;
        _ ->
            file:close(IoDev)
    end.

-spec write_csv(io:device(), dlist()) -> ok.
%%  @doc    Write CSV to specified IO Device.
write_csv(IoDev, Dlist) ->
    write_csv_head(IoDev, Dlist),
    write_csv_body(IoDev, Dlist),
    write_csv_foot(IoDev, Dlist).

-spec write_dot(io:device(), dlist()) -> ok.
%%  @doc    Write graph data to specified IO Device.
write_dot(IoDev, Dlist) ->
    write_dot_head(IoDev, Dlist),
    write_dot_body(IoDev, Dlist),
    write_dot_foot(IoDev, Dlist).

-spec write_txt(io:device(), dlist()) -> ok.
%%  @doc    Write text to specified IO Device.
write_txt(IoDev, Dlist) ->
    write_txt_head(IoDev, Dlist),
    write_txt_body(IoDev, Dlist),
    write_txt_foot(IoDev, Dlist).

-spec collect_confs(plist()) -> clist().
collect_confs(Paths) ->
    collect_confs(Paths, []).

-spec collect_deps(clist()) -> dlist().
collect_deps(Confs) ->
    deps_to_list(collect_deps(Confs, maps:new())).

-spec collect_files(string(), path()) -> plist().
collect_files(Label, TopDir) ->
    RC = filename:join(TopDir, ?REBAR_CFG),
    case filelib:is_regular(RC) of
        true ->
            Top = [{Label, RC}],
            {ok, Terms} = file:consult(RC),
            case proplists:get_value(deps_dir, Terms) of
                undefined ->
                    collect_deps_files(filename:join(TopDir, "deps"), Top);
                Dir ->
                    collect_deps_files(
                        abspath(filename:absname(Dir, TopDir)), Top)
            end;
        _ ->
            collect_dep_files(filename:join(TopDir, "deps"), [])
    end.



-spec abspath(Path :: [string()]) -> path().
%%  @doc    Return the absolute path to a filesystem element with '.'
%%          and '..' resolved.
abspath(Path) ->
    abspath(lists:reverse(filename:split(filename:absname(Path))), []).

%%  @end
%%======================================================================
%%  Internal functions
%%======================================================================

%%
%%  Simple List
%%

write_txt_head(IoDev, _Deps) ->
    io:put_chars(IoDev, ?TXT_HEAD).

write_txt_body(_IoDev, []) ->
    ok;
write_txt_body(IoDev, [{Bflag, Name, {_, Repo}, Vlist} | Deps]) ->
    Own = format_owner(Bflag),
    LC = case length(Vlist) of
        1 ->
            32;
        _ ->
            $!
    end,
    Lead = io_lib:format("~-8.4c", [LC]),
    ok = io:format(IoDev, "~-7s ~-23s ~s\n", [Own, Name, Repo]),
    write_txt_vlist(IoDev, Deps, Lead).

write_txt_vlist(IoDev, [], _Lead) ->
    io:nl(IoDev);
write_txt_vlist(IoDev, [{{RE, Rev}, Deps} | Vrecs], Lead) ->
    ok = io:format(IoDev, "~s~-7s ~-15s ~s~n",
        [Lead, RE, format_rev(Rev), format_dependents(Deps)]),
    write_txt_vlist(IoDev, Vrecs, Lead).

write_txt_foot(_IoDev, _Deps) ->
    ok.

%%
%%  CSV
%%

write_csv_head(IoDev, Deps) ->
    MaxVers = lists:foldl(
        fun({_B, _N, _R, Vlist}, Count) ->
            max(Count, length(Vlist))
        end, 0, Deps),
    ok = io:put_chars(IoDev, "Owner, Package, Repo"),
    ok = lists:foreach(
        fun(N) ->
            ok = io:format(IoDev,
                ", RegEx ~B, Version ~B, Dependencies ~B", [N, N, N])
        end, lists:seq(1, MaxVers)),
    ok = io:nl(IoDev).

write_csv_body(_IoDev, []) ->
    ok;
write_csv_body(IoDev, [{Bflag, Name, {_, Repo}, Vlist} | Deps]) ->
    Own = format_owner(Bflag),
    ok = io:format(IoDev, "\"~s\", \"~s\", \"~s\"", [Own, Name, Repo]),
    ok = write_csv_vlist(IoDev, Vlist),
    ok = io:nl(IoDev),
    write_csv_body(IoDev, Deps).

write_csv_vlist(_IoDev, []) ->
    ok;
write_csv_vlist(IoDev, [{{RE, Rev}, Deps} | Vrecs]) ->
    ok = io:format(IoDev, ", \"~s\", \"~s\", \"~s\"",
        [RE, format_rev(Rev), format_dependents(Deps)]),
    write_csv_vlist(IoDev, Vrecs).

write_csv_foot(_IoDev, _Deps) ->
    ok.

%%
%%  Dot
%%

write_dot_head(IoDev, _Deps) ->
    ok = io:put_chars(IoDev, "digraph RebarDeps {\n").

write_dot_body(IoDev, Deps) ->
    lists:foreach(
        fun({_Bflag, Name, _, Vlist}) ->
            lists:foreach(
                fun({{_RE, _Rev}, Dlist}) ->
                    lists:foreach(
                        fun(Dep) ->
                            ok = io:format(IoDev, "\t~s -> ~s;\n", [Dep, Name])
                        end, Dlist)
                end, Vlist)
        end, Deps).

write_dot_foot(IoDev, _Deps) ->
    ok = io:put_chars(IoDev, "}\n").

%%
%%  Formatters
%%

format_dependents(Deps) ->
    string:join(Deps, "  ").

format_owner(true)      -> "Basho";
format_owner(false)     -> "Extern".

format_rev({tag, S})    -> "t: " ++ S;
format_rev({branch, S}) -> "b: " ++ S;
format_rev({A, S})      -> io_lib:format("\t~s: ~s", [A, S]);
format_rev([])          -> "HEAD";
format_rev(S)           -> S.

%%
%%  Collectors
%%

-spec deps_to_list(dmap()) -> dlist().
deps_to_list(Dmap) ->
    deps_to_list(maps:to_list(Dmap), []).

-spec deps_to_list([tuple()], dlist()) -> dlist().
deps_to_list([], Dlist) ->
    lists:sort(Dlist);
deps_to_list([{{Name, Repo}, {Bflag, Vmap}} | Deps], Dlist) ->
    deps_to_list(Deps, Dlist ++ [{Bflag, Name, Repo, maps:to_list(Vmap)}]).

-spec collect_deps(clist(), dmap()) -> dmap().
collect_deps([], Deps) ->
    Deps;
collect_deps([{Package, Crecs} | Confs], Deps) ->
    collect_deps(Confs, collect_deps(Package, Crecs, Deps)).

-spec collect_deps(name(), [crec()], dmap()) -> dmap().
collect_deps(_, [], Deps) ->
    Deps;
collect_deps(Package, [Crec | Crecs], Deps) ->
    {Name, Repo, Vrec} = Crec,
    Dkey = {Name, Repo},
    Drec = case maps:get(Dkey, Deps, undefined) of
        undefined ->
            {is_basho(Repo), maps:put(Vrec, [Package], maps:new())};
        {Bflag, Users} ->
            Prec = case maps:get(Vrec, Users, undefined) of
                undefined ->
                    [Package];
                Plist ->
                    Plist ++ [Package]
            end,
            {Bflag, maps:put(Vrec, Prec, Users)}
    end,
    collect_deps(Package, Crecs, maps:put(Dkey, Drec, Deps)).

-spec collect_confs(plist(), clist()) -> clist().
collect_confs([], Accum) ->
    Accum;
collect_confs([{Label, Path} | Confs], Accum) ->
    {ok, Terms} = file:consult(Path),
    case proplists:get_value(deps, Terms) of
        undefined ->
            collect_confs(Confs, Accum);
        Dlist ->
            Conf = {Label, collect_conf_deps(Dlist, [])},
            collect_confs(Confs, Accum ++ [Conf])
    end.

collect_conf_deps([], Crecs) ->
    Crecs;
collect_conf_deps([Dep | Deps], Crecs) ->
    case Dep of
        {Pkg, RE, {RT, RP, Rev}} ->
            Crec = {atom_to_list(Pkg), {RT, normalize_repo(RP)}, {RE, Rev}},
            collect_conf_deps(Deps, Crecs ++ [Crec]);
        Drec ->
            error({baddep, Drec})
    end.

-spec collect_deps_files(path(), plist()) -> plist().
collect_deps_files(DepsDir, Accum) ->
    case filelib:is_dir(DepsDir) of
        true ->
            collect_dep_files(
                filelib:wildcard(DepsDir ++ "/*/" ++ ?REBAR_CFG), Accum);
        _ ->
            Accum
    end.

-spec collect_dep_files([path()], plist()) -> plist().
collect_dep_files([], Accum) ->
    Accum;
collect_dep_files([Conf | Confs], Accum) ->
    Label = filename:basename(filename:dirname(Conf)),
    collect_dep_files(Confs, Accum ++ [{Label, Conf}]).

%%
%%  Pattern matching
%%

-spec is_basho({atom(), url()}) -> boolean().
is_basho({git, Url}) ->
    re:run(Url, "://github.com/basho/", [{capture, none}]) == match;
is_basho(_) ->
    false.

-spec normalize_repo(url()) -> url().
normalize_repo("git@github.com:/" ++ Path) ->
    "git://github.com/" ++ Path;
normalize_repo("git@github.com:" ++ Path) ->
    "git://github.com/" ++ Path;
normalize_repo(Path) ->
    Path.

%%
%% Command line handling
%%

-spec usage() -> no_return().
usage() ->
    io:put_chars(standard_error, ?USAGE),
    erlang:halt(1).

-spec parse_args([string()]) -> options() | no_return().
parse_args(Args) ->
    parse_args(Args, maps:new()).

-spec finish_args([string()], options()) -> options() | no_return().
finish_args([], M0) ->
    M1 = case maps:is_key(top, M0) of
        true ->
            M0;
        _ ->
            {ok, CWD} = file:get_cwd(),
            maps:put(top, CWD, M0)
    end,
    M2 = case maps:is_key(label, M1) of
        true ->
            M1;
        _ ->
            L = filename:basename(maps:get(top, M1)),
            maps:put(label, L, M1)
    end,
    M3 = case maps:is_key(format, M2) of
        true ->
            M2;
        _ ->
            F = case maps:is_key(out, M2) of
                true ->
                    X = string:to_lower(filename:extension(maps:get(out, M2))),
                    case X of
                        ".csv" ->
                            csv;
                        ".dot" ->
                            dot;
                        _ ->
                            txt
                    end;
                _ ->
                    txt
            end,
            maps:put(format, F, M2)
    end,
    M3;
finish_args(_, _) ->
    io:put_chars(standard_error, "Error: extra parameters on command line\n"),
    usage().

-spec parse_args([string()], options()) -> options() | no_return().
parse_args([], Accum) ->
    finish_args([], Accum);

parse_args(["-f"], _) -> missing_value("-f");
parse_args(["-n"], _) -> missing_value("-n");
parse_args(["-o"], _) -> missing_value("-o");

parse_args(["-f", "text" | Opts], Accum) ->
    parse_args(Opts, maps:put(format, txt, Accum));
parse_args(["-f", "txt" | Opts], Accum) ->
    parse_args(Opts, maps:put(format, txt, Accum));
parse_args(["-f", "csv" | Opts], Accum) ->
    parse_args(Opts, maps:put(format, csv, Accum));
parse_args(["-f", "dot" | Opts], Accum) ->
    parse_args(Opts, maps:put(format, dot, Accum));
parse_args(["-f" | _], _) ->
    io:put_chars(standard_error, "Error: unrecognized format\n"),
    usage();
parse_args([[$-, $f | Opt] | Opts], Accum) ->
    parse_args(["-f", Opt] ++ Opts, Accum);

parse_args(["-n", Label | Opts], Accum) ->
    parse_args(Opts, maps:put(label, Label, Accum));
parse_args([[$-, $n | Opt] | Opts], Accum) ->
    parse_args(["-n", Opt] ++ Opts, Accum);

parse_args(["-o", Path | Opts], Accum) ->
    Abs = abspath(Path),
    Dir = filename:dirname(Abs),
    case filelib:is_dir(Dir) of
        true ->
            parse_args(Opts, maps:put(out, Abs, Accum));
        _ ->
            io:fwrite(standard_error,
                "Error: output path '~s' not in a directory~n", [Dir]),
            usage()
    end;
parse_args([[$-, $o | Opt] | Opts], Accum) ->
    parse_args(["-o", Opt] ++ Opts, Accum);

parse_args([[$-|_] = Opt|_], _) ->
    io:fwrite(standard_error, "Error: unrecognized option '~s'~n", [Opt]),
    usage();

parse_args([Path | Opts], Accum) ->
    Abs = abspath(Path),
    Top = case string:to_lower(filename:basename(Abs)) of
        ?REBAR_CFG ->
            filename:dirname(Abs);
        _ ->
            case filelib:is_dir(Abs) of
                true ->
                    Abs;
                _ ->
                    io:fwrite(standard_error,
                        "Error: path '~s' is not a directory~n", [Abs]),
                    usage()
            end
    end,
    finish_args(Opts, maps:put(top, Top, Accum)).

-spec missing_value(string()) -> no_return().
missing_value(Opt) ->
    io:fwrite(standard_error, "Error: option '~s' requires a value~n", [Opt]),
    usage().

-spec abspath([string()], [string()]) -> path().
abspath([], Accum) ->
    filename:join(Accum);
abspath(["." | Upper], Accum) ->
    abspath(Upper, Accum);
abspath(["..", LastIsRoot], Accum) ->
    % special case to handle parent of root directory
    abspath([LastIsRoot], Accum);
abspath(["..", _Intermediate | Upper], Accum) ->
    abspath(Upper, Accum);
abspath([Cur | Upper], Accum) ->
    abspath(Upper, [Cur] ++ Accum).

%%  @end
%%======================================================================
%%  EUnit Tests
%%======================================================================
-ifdef(TEST).


-endif. % TEST

