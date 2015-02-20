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

-module(scrape_rebar).

-export([
    write_list/0, write_list/1, write_list/2,
    write_csv/0, write_csv/1, write_csv/2,
    collect_confs/1,
    collect_deps/1,
    get_conf_files/0, get_conf_files/1,
    abspath/1
]).

-type path()    :: file:filename().
-type plist()   :: [path()].

-type name()    :: atom().
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

%%
%%  Simple List
%%

write_list() ->
    list_io(get_conf_files(), standard_io).

write_list(TopDir) ->
    list_io(get_conf_files(TopDir), standard_io).

write_list(TopDir, OutFile) ->
    {ok, FD} = file:open(OutFile, [write]),
    print_head1(FD),
    list_io(get_conf_files(TopDir), FD),
    file:close(FD).

list_io(InFiles, IoDev) ->
    print_deps1(collect_deps(collect_confs(InFiles)), IoDev).

%%
%%  CSV
%%

write_csv() ->
    csv_io(get_conf_files(), standard_io).

write_csv(TopDir) ->
    csv_io(get_conf_files(TopDir), standard_io).

write_csv(TopDir, OutFile) ->
    {ok, FD} = file:open(OutFile, [write]),
    csv_io(get_conf_files(TopDir), FD),
    file:close(FD).

csv_io(InFiles, IoDev) ->
    Deps = collect_deps(collect_confs(InFiles)),
    print_head2(Deps, IoDev),
    print_deps2(Deps, IoDev).

%%
%%  Formatters
%%

print_deps1([], _IoDev) ->
    ok;
print_deps1([Dep | Deps], IoDev) ->
    print_dep1(Dep, IoDev),
    print_deps1(Deps, IoDev).

print_head1(IoDev) ->
    io:put_chars(IoDev,
"#=======================================================================
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
#=======================================================================

").

print_deps2([], _IoDev) ->
    ok;
print_deps2([Dep | Deps], IoDev) ->
    print_dep2(Dep, IoDev),
    print_deps2(Deps, IoDev).

print_head2(Deps, IoDev) ->
    io:put_chars(IoDev, "Owner, Package, Repo"),
    [print_vhead2(IoDev, N) || N <- lists:seq(1, max_versions(Deps))],
    io:nl(IoDev).

print_vhead2(IoDev, N) ->
    io:format(IoDev, ", RegEx ~B, Version ~B, Dependencies ~B", [N, N, N]).

max_versions(Deps) ->
    lists:foldl(
        fun({_B, _N, _R, Vlist}, Count) ->
            max(Count, length(Vlist))
        end, 0, Deps).

print_dep2({Bflag, Name, {_, Repo}, Vlist}, IoDev) ->
    Own = case Bflag of
        true ->
            "Basho";
        _ ->
            "Extern"
    end,
    ok = io:format(IoDev, "\"~s\", \"~s\", \"~s\"", [Own, Name, Repo]),
    print_vlist2(Vlist, IoDev),
    io:nl(IoDev).

print_vlist2([], _IoDev) ->
    ok;
print_vlist2([{{RE, Rev}, Users} | Vrecs], IoDev) ->
    ok = io:format(IoDev, ", \"~s\", \"~s\", \"~s\"",
        [RE, format_rev1(Rev), format_users1(Users)]),
    print_vlist2(Vrecs, IoDev).

print_dep1({Bflag, Name, {_, Repo}, Vlist}, IoDev) ->
    Own = case Bflag of
        true ->
            "Basho";
        _ ->
            "Extern"
    end,
    LC = case length(Vlist) of
        1 ->
            32;
        _ ->
            $!
    end,
    Lead = io_lib:format("~-8.4c", [LC]),
    ok = io:format(IoDev, "~-7s ~-23s ~s\n", [Own, Name, Repo]),
    print_vlist1(Vlist, IoDev, Lead).

print_vlist1([], IoDev, _Lead) ->
    io:nl(IoDev);
print_vlist1([{{RE, Rev}, Users} | Vrecs], IoDev, Lead) ->
    io:format(IoDev, "~s~-7s ~-15s ~s\n",
        [Lead, RE, format_rev1(Rev), format_users1(Users)]),
    print_vlist1(Vrecs, IoDev, Lead).

format_users1(Users) ->
    string:join(Users, "  ").

format_rev1({tag, S})    -> "t: " ++ S;
format_rev1({branch, S}) -> "b: " ++ S;
format_rev1({A, S})      -> io_lib:format("\t~s: ~s", [A, S]);
format_rev1([])          -> "HEAD";
format_rev1(S)           -> S.

%%
%%  Collectors
%%

-spec collect_confs(plist()) -> clist().
collect_confs(Paths) ->
    collect_confs(Paths, []).

-spec collect_confs(plist(), clist()) -> clist().
collect_confs([], Confs) ->
    Confs;
collect_confs([Path | Paths], Confs) ->
    {ok, Terms} = file:consult(get_path_file(Path)),
    case proplists:get_value(deps, Terms) of
        undefined ->
            collect_confs(Paths, Confs);
        Dlist ->
            Conf = {get_path_label(Path), collect_conf_deps(Dlist, [])},
            collect_confs(Paths, Confs ++ [Conf])
    end.

get_path_file({_Label, File}) ->
    File;
get_path_file(Path) ->
    Path.
get_path_label({Label, _File}) when is_list(Label) ->
    Label;
get_path_label({Label, _File}) ->
    io_lib:format("~s", [Label]);
get_path_label(Path) ->
    filename:basename(filename:dirname(Path)).

collect_conf_deps([], Crecs) ->
    Crecs;
collect_conf_deps([Dep | Deps], Crecs) ->
    case Dep of
        {Name, RE, {Proto, Repo, Rev}} ->
            Crec = {Name, {Proto, normalize_repo(Repo)}, {RE, Rev}},
            collect_conf_deps(Deps, Crecs ++ [Crec]);
        D ->
            error({baddep, D})
    end.

% quick and dirty
normalize_repo([$g, $i, $t, $@, $g, $i, $t, $h, $u, $b, $., $c, $o, $m, $:, $/ | Path]) ->
    "git://github.com/" ++ Path;
normalize_repo([$g, $i, $t, $@, $g, $i, $t, $h, $u, $b, $., $c, $o, $m, $: | Path]) ->
    "git://github.com/" ++ Path;
normalize_repo(Path) ->
    Path.

-spec collect_deps(clist()) -> dlist().
collect_deps(Confs) ->
    deps_to_list(collect_deps(Confs, maps:new())).

deps_to_list(Dmap) ->
    deps_to_list(maps:to_list(Dmap), []).

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

is_basho({git, Url}) ->
    re:run(Url, "://github.com/basho/", [{capture, none}]) == match;
is_basho(_) ->
    false.

get_conf_files() ->
    {ok, AbsDir} = file:get_cwd(),
    get_conf_files_abs(AbsDir).

get_conf_files({Label, TopDir}) ->
    get_conf_files_abs(Label, abspath(TopDir));
get_conf_files(TopDir) ->
    get_conf_files_abs(abspath(TopDir)).

get_conf_files_abs(AbsDir) ->
    get_conf_files_abs(filename:basename(AbsDir), AbsDir).

get_conf_files_abs(Label, AbsDir) ->
    RC = filename:join(AbsDir, "rebar.config"),
    L1 = case filelib:is_regular(RC) of
        true ->
            [{Label, RC}];
        _ ->
            []
    end,
    DD = filename:join(AbsDir, "deps"),
    L2 = case filelib:is_dir(DD) of
        true ->
            filelib:wildcard(DD ++ "/*/rebar.config");
        _ ->
            []
    end,
    L1 ++ L2.

abspath(Path) ->
    abspath(lists:reverse(filename:split(filename:absname(Path))), []).

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

