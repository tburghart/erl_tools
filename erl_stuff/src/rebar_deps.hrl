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

%%  The trailing number is random, it exists only to avoid guard conflicts.
-ifndef(REBAR_DEPS_HRL_INCLUDED_47992).
-define(REBAR_DEPS_HRL_INCLUDED_47992, true).

-define(OPTIONS, [label, out, format, top]).
-define(FORMATS, [txt, csv, dot]).

-define(REBAR_CFG, "rebar.config").

-define(SCRIPT_ABS, abspath(escript:script_name())).
-define(SCRIPT_DIR, filename:dirname(?SCRIPT_ABS)).
-define(SCRIPT_NAME, filename:basename(escript:script_name())).

-define(USAGE,
"Usage: " ++ ?SCRIPT_NAME
++ " [-n <label>] [-o <outfile>] [-f {text|txt|csv|dot}] [<starting-path>]
where
    -n <label> specifies the name to use for the top-level package. If not
        specified, the unqualified name of the starting directory is used.
    -o <outfile> specifies the path to a file where output will be
        [over]written. If not specified, output is written to standard output.
    -f <format> specifies the output format. If not specified, and the -o
        option is provided, the extension of the output file is used if it
        matches one of the recognized format specifiers. If neither of the
        above yields a useable format, the default 'text' format is written.
    <starting-path> specifies a directory or rebar.config file that serves as
        the top-level package anchor. If not specified, the current working
        directory is used.

If any other parameter starting with '-' is provided (such as -h or --help),
This usage message is printed to standard error and a non-zero result is
returned.

On success, the script returns zero.
").

-define(TXT_HEAD,
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

-define(DOT_HEAD, "").

-endif. % REBAR_DEPS_HRL_INCLUDED_47992
