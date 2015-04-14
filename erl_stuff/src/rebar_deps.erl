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

-type option()  :: format | label | out | top.
-type optval()  :: atom() | string().
-type name()    :: string().
-type url()     :: string().
-type proto()   :: atom().
-type repo()    :: {proto(), url()}.
-type regex()   :: string().
-type rev()     :: {atom(), string()} | string().
-type vrec()    :: {regex(), rev()}.
-type usedl()   :: {vrec(), [name()]}.
-type usedm()   :: #{vrec() => [name()]}.
-type basho()   :: boolean().

-type drecl()   :: {basho(), [usedl()]}.
-type drecm()   :: {basho(), [usedm()]}.

-type dmap()    :: #{name() => drecm()}.
-type dlist()   :: [{name(), drecl()}].

-type crec()    :: {name(), repo(), vrec()}.
-type conf()    :: {name(), [crec()]}.
-type clist()   :: [conf()].

%%======================================================================
%%  Macros
%%======================================================================

-define(REBAR_CFG,      "rebar.config").

-define(SCRIPT_ABS,     abspath(escript:script_name())).
-define(SCRIPT_DIR,     filename:dirname(?SCRIPT_ABS)).
-define(SCRIPT_NAME,    filename:basename(escript:script_name())).

-define(DOT_NODE_SHAPE, "box").
-define(DOT_COLOR_DFLT, "blue").
-define(DOT_COLOR_SING, "green").
-define(DOT_COLOR_MULT, "red").

%%======================================================================
%%  API functions
%%======================================================================

-spec main([string()]) -> ok | no_return().
%%
%%  @doc    Escript main.
%%
%%  Invocation options are displayed by providing `-h` as a parameter.
%%
main(Args) ->
    ok    = parse_args(Args),
    Files = collect_files(get_opt(label), get_opt(top)),
    Confs = collect_confs(Files),
    Deps  = collect_deps(Confs),
    IoDev = case get_opt(out) of
        undefined ->
            standard_io;
        OutFile ->
            {ok, FD} = file:open(OutFile, [write]),
            FD
    end,
    % It's tempting to resolve the function dynamically, since we know the
    % pattern, but if running interpretted from escript the functions won't
    % be found.
    case get_opt(format) of
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
%%
%%  @doc    Write CSV to specified IO Device.
%%
write_csv(IoDev, Dlist) ->
    write_csv_head(IoDev, Dlist),
    write_csv_body(IoDev, Dlist),
    write_csv_foot(IoDev, Dlist).

-spec write_dot(io:device(), dlist()) -> ok.
%%
%%  @doc    Write graph data to specified IO Device.
%%
write_dot(IoDev, Dlist) ->
    write_dot_head(IoDev, Dlist),
    write_dot_body(IoDev, Dlist),
    write_dot_foot(IoDev, Dlist).

-spec write_txt(io:device(), dlist()) -> ok.
%%
%%  @doc    Write text to specified IO Device.
%%
write_txt(IoDev, Dlist) ->
    write_txt_head(IoDev, Dlist),
    write_txt_body(IoDev, Dlist),
    write_txt_foot(IoDev, Dlist).

-spec collect_confs(plist()) -> clist().
%%
%%  @doc    Returns a list of tuples containing rebar config terms.
%%
%%  Each element of the list is in the form
%%  `{Label, [{Package, {Proto, Repo}, {RegEx, Rev}}]}`
%%  where `Label` is the name of a package and the associated list contains
%%  one tuple for each package that depends on it.
%%
collect_confs(Paths) ->
    collect_confs(Paths, []).

-spec collect_deps(clist()) -> dlist().
%%
%%  @doc    Returns a list of tuples containing rebar dependency information.
%%
%%  Each element of the list is in the form
%%  `{Bflag, Label, {Proto, Repo}, [{{RegEx, Rev}, [Dependent]}]}`
%%
%%
collect_deps(Confs) ->
    deps_to_list(collect_deps(Confs, maps:new())).

-spec collect_files(string(), path()) -> plist().
%%
%%  @doc    Returns a list of tuples containing rebar config file information.
%%
%%  Each element of the list is in the form `{Label, Path}`, where `Label`
%%  is the string representing the package name and `Path` is the absolute
%%  path to a rebar config file.
%%
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
                    collect_deps_files(abspath(Dir, TopDir), Top)
            end;
        _ ->
            collect_deps_files(filename:join(TopDir, "deps"), [])
    end.

-spec abspath(Path :: string()) -> path().
%%
%%  @doc    Return the absolute path to a filesystem element with '.'
%%          and '..' resolved.
%%
%%  If the specified path does not start with the platform's representation
%%  of a 'root' directory, it is resolved relative to the current working
%%  directory.
%%
%%  This operation is completed entirely through text manipulation without
%%  side effects (the CWD may be obtained if necessary, but will not be
%%  changed). As such, elements of the resulting path may not exist, and
%%  whether the result contains symbolic links is not defined.
%%
%%  The result is equivalent to that of {@link filename:absname/1} with all
%%  relative path segments resolved.
%%
abspath(Path) ->
    abspath_inner(lists:reverse(filename:split(filename:absname(Path))), []).

-spec abspath(Path :: string(), Base :: path()) -> path().
%%
%%  @doc    Return the absolute path to a filesystem element with '.'
%%          and '..' resolved.
%%
%%  If the specified `Path` does not start with the platform's representation
%%  of a 'root' directory, it is resolved relative to the provided `Base`
%%  directory, which is itself resolved as if by invoking {@link abspath/1}.
%%
%%  This operation is completed entirely through text manipulation without
%%  side effects (the CWD may be obtained if necessary, but will not be
%%  changed). As such, elements of the resulting path may not exist, and
%%  whether the result contains symbolic links is not defined.
%%
%%  The result is equivalent to that of {@link filename:absname/2} with all
%%  relative path segments resolved, and is assured to be absolute even when
%%  the specified `Base` is not.
%%
abspath(Path, Base) ->
    abspath_inner(lists:reverse(filename:split(
        filename:absname(Path, filename:absname(Base)))), []).

%%  @end
%%======================================================================
%%  Internal functions
%%======================================================================

%%
%%  Simple List
%%

write_txt_head(IoDev, _Deps) ->
    io:put_chars(IoDev, txt_header()).

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
    write_txt_vlist(IoDev, Vlist, Lead),
    write_txt_body(IoDev, Deps).

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

write_dot_head(IoDev, Deps) ->
    ok = io:put_chars(IoDev, dot_header()),
    write_dot_attr(IoDev, Deps).

write_dot_attr(IoDev, []) ->
    io:nl(IoDev);
write_dot_attr(IoDev, [{Bflag, Name, _Repo, Vlist} | Deps]) ->
    Color = case length(Vlist) > 1 of
        true ->
            ?DOT_COLOR_MULT;
        _ ->
            ?DOT_COLOR_SING
    end,
    Owner = format_owner(Bflag),
    ok = io:format(IoDev,
        "\t~s [label=\"~s\\n~s\",color=~s]\n",
        [Name, Name, Owner, Color]),
    write_dot_attr(IoDev, Deps).

write_dot_body(_IoDev, []) ->
    ok;
write_dot_body(IoDev, [{_Bflag, Name, _Repo, Vlist} | Deps]) ->
    write_dot_elem(IoDev, Name, Vlist),
    write_dot_body(IoDev, Deps).

write_dot_elem(_IoDev, _Name, []) ->
    ok;
write_dot_elem(IoDev, Name, [{{_RE, _Rev}, Dlist} | Deps]) ->
    write_dot_deps(IoDev, Name, Dlist),
    write_dot_elem(IoDev, Name, Deps).

write_dot_deps(_IoDev, _Name, []) ->
    ok;
write_dot_deps(IoDev, Name, [Dep | Deps]) ->
    ok = io:format(IoDev, "\t~s -> ~s;\n", [Dep, Name]),
    write_dot_deps(IoDev, Name, Deps).

write_dot_foot(IoDev, _Deps) ->
    io:put_chars(IoDev, dot_footer()).

%%
%%  Formatters
%%

format_dependents(Deps) ->
    string:join(Deps, "  ").

format_owner(true)      -> "basho";
format_owner(false)     -> "extern".

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

-spec deps_to_list([{{name(), repo()}, drecm()}], dlist()) -> dlist().
deps_to_list([], Dlist) ->
    lists:sort(Dlist);
% As of Dialyzer 2.7.3 the loosely-typed maps:to_list/1 result causes
% dialyzer to think the result dlist() may not be up to snuff. It's fine.
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
collect_deps(Package, [{Name, Repo, Vrec} | Crecs], Deps) ->
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
        {Pkg, RE, Loc, _Opts} ->
            % strip optional rebar processing options
            collect_conf_deps([{Pkg, RE, Loc}] ++ Deps, Crecs);
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

-spec get_opt(option()) -> optval().
get_opt(Key) ->
    erlang:get({?MODULE, Key}).

-spec set_opt(option(), optval()) -> optval() | undefined.
set_opt(Key, Val) ->
    erlang:put({?MODULE, Key}, Val).

-spec usage() -> no_return().
usage() ->
    io:put_chars(standard_error, usage_text()),
    erlang:halt(1).

-spec finish_args([string()]) -> ok | no_return().
finish_args([]) ->
    case get_opt(top) of
        undefined ->
            {ok, CWD} = file:get_cwd(),
            set_opt(top, CWD);
        _ ->
            ok
    end,
    case get_opt(label) of
        undefined ->
            set_opt(label, filename:basename(get_opt(top)));
        _ ->
            ok
    end,
    case get_opt(format) of
        undefined ->
            case get_opt(out) of
                undefined ->
                    set_opt(format, txt);
                FP ->
                    case string:to_lower(filename:extension(FP)) of
                        ".csv" ->
                            set_opt(format, csv);
                        ".dot" ->
                            set_opt(format, dot);
                        _ ->
                            set_opt(format, txt)
                    end
            end;
        _ ->
            ok
    end,
    ok;
finish_args(_) ->
    io:put_chars(standard_error, "Error: extra parameters on command line\n"),
    usage().

-spec parse_args([string()]) -> ok | no_return().
parse_args([])          -> finish_args([]);
parse_args(["-h"|_])    -> usage();
parse_args(["-f"])      -> missing_value("-f");
parse_args(["-n"])      -> missing_value("-n");
parse_args(["-o"])      -> missing_value("-o");

parse_args(["-f", "text" | Opts]) ->
    set_opt(format, txt),
    parse_args(Opts);
parse_args(["-f", "txt" | Opts]) ->
    set_opt(format, txt),
    parse_args(Opts);
parse_args(["-f", "csv" | Opts]) ->
    set_opt(format, csv),
    parse_args(Opts);
parse_args(["-f", "dot" | Opts]) ->
    set_opt(format, dot),
    parse_args(Opts);
parse_args(["-f" | _]) ->
    io:put_chars(standard_error, "Error: unrecognized format\n"),
    usage();
parse_args([[$-, $f | Opt] | Opts]) ->
    parse_args(["-f", Opt] ++ Opts);

parse_args(["-n", Label | Opts]) ->
    set_opt(label, Label),
    parse_args(Opts);
parse_args([[$-, $n | Opt] | Opts]) ->
    parse_args(["-n", Opt] ++ Opts);

parse_args(["-o", Path | Opts]) ->
    Abs = abspath(Path),
    Dir = filename:dirname(Abs),
    case filelib:is_dir(Dir) of
        true ->
            set_opt(out, Abs),
            parse_args(Opts);
        _ ->
            io:fwrite(standard_error,
                "Error: output path '~s' not in a directory~n", [Dir]),
            usage()
    end;
parse_args([[$-, $o | Opt] | Opts]) ->
    parse_args(["-o", Opt] ++ Opts);

parse_args([[$-|_] = Opt | _]) ->
    io:fwrite(standard_error, "Error: unrecognized option '~s'~n", [Opt]),
    usage();

parse_args([Path | Opts]) ->
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
    set_opt(top, Top),
    finish_args(Opts).

-spec missing_value(string()) -> no_return().
missing_value(Opt) ->
    io:fwrite(standard_error, "Error: option '~s' requires a value~n", [Opt]),
    usage().

-spec abspath_inner([string()], [string()]) -> path().
%%
%%  Assembles an exploded and reversed list of path elements into an
%%  absolute path. Used by abspath/1 and abspath/2, with little utility
%%  otherwise.
%%
abspath_inner([], Accum) ->
    filename:join(Accum);
abspath_inner(["." | Upper], Accum) ->
    abspath_inner(Upper, Accum);
abspath_inner(["..", LastIsRoot], Accum) ->
    % special case to handle parent of root directory
    abspath_inner([LastIsRoot], Accum);
abspath_inner(["..", _Intermediate | Upper], Accum) ->
    abspath_inner(Upper, Accum);
abspath_inner([Cur | Upper], Accum) ->
    abspath_inner(Upper, [Cur] ++ Accum).

%%
%%  Text
%%

-spec usage_text() -> string().
usage_text() ->
"Usage: escript " ++ ?SCRIPT_NAME ++ " [options] [<starting-path>]
The <starting-path> specifies a directory or " ++ ?REBAR_CFG ++ " file that
serves as the top-level package anchor. If not specified, the current
working directory is used.
The following options are recognized:
    -n <label>
        Specifies the name to use for the top-level package. If not specified,
        the unqualified name of the starting directory is used.
    -o <outfile>
        Specifies the path to a file where output will be [over]written. If
        not specified, output is written to standard output.
    -f {text|txt|csv|dot}
        Specifies the output format. If not specified, and the -o option
        is provided, the extension of the output file is used if it matches
        one of the recognized format specifiers. If neither of the above
        yields a useable format, the default 'text' format is written.
    -h
        This text is written to standard error and a non-zero result is
        returned.

If any other parameter starting with '-' is provided, or any parameters are
found following the <starting-path>, an error message is written to standard
error, followed by this text, and a non-zero result is returned.

On success, the script returns zero.
".

-spec txt_header() -> string().
txt_header() ->
"#=============================================================================
# Rebar dependencies
#
# Each entry is comprised of one line describing the package and one or more
# lines detailing where it's used. The package description line consists of:
#
#   Owner  PackageName  Repository
#
# The usage lines are grouped by the revision specified in the rebar.config
# file(s) listing the package as a dependency, and consist of:
#
#   RegEx  Revision  Dependent  [Dependent ...]
#
# If there are dependents that specify differing revisions (or RegEx revision
# filters) they are grouped on separate lines, and each line is preceded with
# a string of exclamation points.
#=============================================================================

".

-spec dot_header() -> string().
dot_header() ->
    TL = get_opt(label),
"#
# Dependencies of " ++ TL ++ "'s " ++ ?REBAR_CFG ++ " file.
# Generated by " ++ ?SCRIPT_NAME ++ "
# Colors:
#   " ++ ?DOT_COLOR_DFLT ++ " represents the top-level node.
#   " ++ ?DOT_COLOR_SING ++ " represents a consistently versioned dependency.
#   " ++ ?DOT_COLOR_MULT ++ " represents an inconsistently versioned dependency.
#
digraph \"" ++ TL ++ " dependencies\"
{
\tnode [shape=" ++ ?DOT_NODE_SHAPE ++ ",color=" ++ ?DOT_COLOR_DFLT ++ "];

".

-spec dot_footer() -> string().
dot_footer() ->
"}
".

%%  @end
%%======================================================================
%%  EUnit Tests
%%======================================================================
-ifdef(TEST).


-endif. % TEST

