:- module(ci,
          [ test_base/3,     % +OS,+Tag,-Base
            docker_pull/2,   % +OS,+Tag
            test/6,          % +OS,+Tag,+Type,+Branch,+PackageBranches,+Configs
            current_test/3   % ?OS,?Tag,-Configs
          ]).
:- use_module(library(apply), [maplist/2, maplist/3]).
:- use_module(library(dicts), [dict_keys/2]).
:- use_module(library(filesex), [directory_file_path/3, directory_member/3]).
:- use_module(library(lists), [selectchk/3, append/2, append/3]).
:- use_module(library(option), [option/2]).
:- use_module(library(pairs), [pairs_values/2]).
:- use_module(library(process), [process_create/3, process_wait/2]).
:- use_module(library(yaml), [yaml_read/2]).
:- use_module(library(error), [must_be/2]).
:- use_module(library(broadcast), [broadcast/1]).
:- use_module(library(dcg/basics), [float/3, whites/2, nonblanks/3]).
:- use_module(config).

:- multifile
    user:file_search_path/2.

user:file_search_path(share, share).

%!  current_test(?OS, ?Tag, -Configs) is nondet.
%
%   Enumerate all available test configurations. Configs   is  a list of
%   config ids as provided by `ci.yaml` in the OS/Tag directory.

current_test(OS, Tag, Configs) :-
    directory_member(., OS, [file_type(directory)]),
    directory_member(OS, TagDir, [file_type(directory)]),
    file_base_name(TagDir, Tag),
    directory_file_path(TagDir, 'ci.yaml', CI),
    exists_file(CI),
    config_dict(TagDir, ConfigDict),
    maplist(yaml_config, ConfigDict.configs, Configs).

yaml_config(ConfigDict, Config) :-
    dict_keys(ConfigDict, [Config]).


%!  test_base(+OS, +Tag, -Base) is det.
%
%   Create the base docker image for OS:Tag   that  has the docker image
%   name Base.

test_base(OS, Tag, Base) :-
    prepare_context(OS, Tag),
    docker_base_file(OS, Tag, Dockerfile),
    base_image(OS, Tag, Base),
    docker_build(Dockerfile, Base).

docker_base_file(OS, Tag, Dockerfile) :-
    os_dir(OS, Tag, Dir),
    directory_file_path(Dir, 'Dockerfile.base', Dockerfile),
    setup_call_cleanup(
        open(Dockerfile, write, Out),
        generate_docker_file(Out, OS, Tag),
        close(Out)).

generate_docker_file(Out, OS, Tag) :-
    os_dir(OS, Tag, Dir),
    copy_file_to(Out, share('Header.docker')),
    directory_file_path(Dir, 'Dependencies.docker', Deps),
    copy_file_to(Out, Deps),
    docker_ignore_cache(Out),
    copy_file_to(Out, share('Scripts.docker')),
    copy_file_to(Out, share('Download.docker')),
    config_dict(Dir, Config),
    maplist(docker_config(Out, OS, Tag, Config.default, _, _, docker),
            Config.configs).

docker_ignore_cache(Out) :-
    format(Out, '# Force ignoring the Docker cache~n', []),
    get_time(Now),
    docker_command(Out,
                   [ [ echo, Now ]
                   ]).



config_dict(Dir, Config) :-
    directory_file_path(Dir, 'ci.yaml', CI),
    yaml_read(CI, Config).

%! docker_config(+Out, +OS, +Tag, +DefaultDict, +Steps, +Configs,
%!               +Format, +ConfigDict)

docker_config(Out, OS, Tag, Defaults, Steps, Configs, Format, ConfigDict) :-
    dict_keys(ConfigDict, [Config]),
    (   Configs == all
    ->  true
    ;   memberchk(Config, Configs)
    ),
    !,
    atom_concat('build.', Config, BuildDir),
    config_options(Defaults, ConfigDict.Config, Options),
    format(string(BuildDirArg), "$SWIPL_SRC/~w", [BuildDir]),

    Date = "$(date +%s.%N)",
    GitVersion = "$(git describe)",
    Branch = "$(git rev-parse --abbrev-ref HEAD)",
    CC = "$(/opt/bin/cc-version)",

    Start =     [ [ echo, '@@START:', OS, Tag, Config, Date, Branch, GitVersion ]
                ],

    Configure = [ [ cd, "$SWIPL_SRC" ],
                  [ rm, '-rf', BuildDir ],
                  [ mkdir, BuildDir],
                  [ cd, BuildDir ],
                  [ cmake, +Options.cmake_flags,
                           '-G', Options.generator.cmake,
                           '..'
                  ] - Options.cmake_env,
                  [ echo, '@@PASSED:', OS, Tag, Config, Date, Branch, GitVersion, CC, configure ]
                ],

    Build =     [ [ cd, BuildDirArg ],
                  [ +Options.generator.command ],
                  [ echo, '@@PASSED:', OS, Tag, Config, Date, Branch, GitVersion, CC, build ]
                ],

    Test =	[ [ cd, BuildDirArg ],
                  [ ctest, '-j', '$(nproc)', '--output-on-failure'
                  ] - Options.ctest_env,
                  [ echo, '@@PASSED:', OS, Tag, Config, Date, Branch, GitVersion, CC, test ]
                ],

    Bench =	[ [ cd, BuildDirArg ],
                  [ echo, '@@BENCH:', begin ],
                  [ 'src/swipl', '../bench/run.pl', '--csv' ],
                  [ echo, '@@BENCH:', end, bench, OS, Tag, Config, Date, Branch, GitVersion, CC ]
                ],

    docker_comment(Out, '~N~n# Configuration ~w: ~s~n',
                   [Config, Options.comment], Format),
    if_step(configure, Steps, Configure, C1),
    if_step(build,     Steps, Build,     C2),
    if_step(bench,     Steps, Bench,     C3),
    if_step(test,      Steps, Test,      C4),
    append([Start,C1,C2,C3,C4], Commands),
    docker_command(
        Out, Commands,
        [ echo, '@@FAILED:', OS, Tag, Config, Date, Branch, GitVersion, CC ],
        Format).
docker_config(_Out, _OS, _Tag, _Defaults, _Steps, _Configs, _Format, _ConfigDict).

if_step(Step, Steps, Commands0, Commands) :-
    (   memberchk(Step, Steps)
    ->  Commands = Commands0
    ;   Commands = []
    ).

config_options(Defaults, "", Options) =>
    config_options(Defaults, yaml{}, Options).
config_options(Defaults, Config, Options) =>
    findall(Name-Value, config(Name, Defaults, Config, Value), Pairs),
    dict_pairs(Options, _, Pairs).

config(Name, Defaults, Config, Value) :-
    config(Name, append, Sep),
    atomics_to_string([ Defaults.get(Name, ""),
                        Config.get(Name, "")
                      ], Sep, Value).
config(Name, Defaults, Config, Value) :-
    config(Name, join, Initial),
    Value0 = Initial.put(Defaults.get(Name, _{})),
    Value  = Value0.put(Config.get(Name, _{})).
config(Name, Defaults, Config, Value) :-
    config(Name, override, Default),
    (   Value = Config.get(Name)
    ->  true
    ;   Value = Defaults.get(Name)
    ->  true
    ;   nonvar(Default)
    ->  Value = Default
    ).

config(generator,   override, _{cmake:"Ninja", command:ninja}).
config(cmake_flags, append,   " ").
config(cmake_env,   join,     _{}).
config(ctest_env,   join,     _{}).
config(comment,     override, "no comment").


		 /*******************************
		 *      INCREMENTAL BUILDS	*
		 *******************************/

%!  test(+OS, +Tag, +Type, +Branch, +PackageBranches, +Configs) is semidet.
%
%   Run  a  set  of  tests  on  OS/Tag.   Type  is  one  of  `clean`  or
%   `incremetal`. Branch is the branch to  test and PackageBranches is a
%   dict that can be used to point   at  alternative branches for one or
%   more of the packages. For example _{clib:testing}. Configs is either
%   `all` or a list of configurations defined  in `ci.yaml` to test. The
%   list may contain configurations that  are   not  defined in ci.yaml.
%   Such configurations are ignored.

test(OS, Tag, Type, Branch, PackageBranches, all) =>
    current_test(OS, Tag, Configs),
    test(OS, Tag, Type, Branch, PackageBranches, Configs).
test(OS, Tag, Type, Branch, PackageBranches, Configs) =>
    must_be(oneof([clean,incremental]), Type),
    maplist(test_config(OS, Tag, Type, Branch, PackageBranches),
            Configs).

test_config(OS, Tag, Type, Branch, PackageBranches, Config) :-
    with_output_to(
        string(Command),
        test_command(current_output,
                     OS, Tag, Type, Branch, PackageBranches, Config)),
    os_dir(OS, Tag, Dir),
    build_id(OS, Tag, Config, BuildID),
    base_image(OS, Tag, Base),
    format(atom(LogFile), '~w/~w.log', [Dir, BuildID]),
    docker_run(Dir,
               [run, '-i', '--rm', Base, '/bin/bash', '-c', Command],
               LogFile).

test_command(Out, OS, Tag, Type, Branch, PackageBranches, Config) :-
    checkout_version_command(Out, Branch, PackageBranches),
    connect(Out, '&&', shell),
    os_dir(OS, Tag, Dir),
    config_dict(Dir, OSConfig),
    type_steps(Type, Steps),
    maplist(docker_config(Out, OS, Tag, OSConfig.default,
                          Steps,
                          [Config], shell), OSConfig.configs).

build_id(OS, Tag, Config, BuildID) :-
    redis(ci, incr(build:OS:Tag:Config), Num),
    format(atom(BuildID), 'swipl-~w-~w-~w-build-~w', [OS, Tag, Config, Num]).

type_steps(incremental, [build, bench, test]).
type_steps(clean,       [configure, build, bench, test]).

checkout_version_command(Out, Branch, PackageBranches) :-
    (   atomic_list_concat([Remote, _RBranch], /, Branch)
    ->  true
    ;   Remote = origin
    ),
    (   remote(Remote, RemoteURL)
    ->  true
    ;   existence_error(remote, RemoteURL)
    ),
    dict_pairs(PackageBranches, _, Pairs),
    maplist(pkg_checkout_cmd, Pairs, Cmds),
    append(Cmds, PkgsCmd),
    docker_command(Out,
                   [ [ '/opt/bin/swipl-checkout',
                       '-r', Remote, RemoteURL,
                       '-b', Branch
                     | PkgsCmd
                     ]
                   ], [], shell).

pkg_checkout_cmd(Pkg-Branch, ['-p', Pkg, Branch]).


		 /*******************************
		 *           PROCESSES		*
		 *******************************/

%!  docker_command(+Out, +Command, +Or, +Format)

docker_command(Out, Command) :-
    docker_command(Out, Command, [], docker).

docker_command(Out, Command, Or, docker) =>
    format(Out, '~N~nRUN\t', []),
    docker_command_lines(Out, Command, Or, docker).
docker_command(Out, Command, Or, shell) =>
    docker_command_lines(Out, Command, Or, shell).

docker_command_lines(_, [], _, _) :-
    !.
docker_command_lines(Out, [H|T], Or, Format) :-
    docker_command_line(Out, H),
    (   T == []
    ->  (   Or == []
        ->  command_end(Out, Format)
        ;   connect(Out, '||', Format),
            docker_command_line(Out, Or),
            nl(Out)
        )
    ;   connect(Out, '&&', Format),
        docker_command_lines(Out, T, Or, Format)
    ).

connect(Out, Connector, docker) =>
    format(Out, ' ~w \\~n~t', [Connector]).
connect(Out, Connector, shell) =>
    format(Out, ' ~w ', [Connector]).

command_end(Out, docker) =>
    nl(Out).
command_end(_Out, shell) =>
    true.

docker_command_line(Out, Parts-Env) :-
    !,
    dict_pairs(Env, _, Pairs),
    maplist(env_command(Out), Pairs),
    docker_command_line(Out, Parts).
docker_command_line(Out, Parts) :-
    maplist(shell_quote, Parts, QParts),
    atomics_to_string(QParts, " ", Line),
    format(Out, '~s', [Line]).

%!  shell_quote(+In, -Quoted) is det.
%
%   Quote arguments for the shell.   Bit simple for now.

shell_quote(Arg, Arg) :-
    number(Arg),
    !.
shell_quote(+Arg, Arg) :-
    !.
shell_quote(Arg, QArg) :-
    split_string(Arg, " ()[]?", "", [_,_|_]),
    !,
    format(string(QArg), '"~w"', Arg).
shell_quote(Arg, Arg).

env_command(Out, Name-Value) :-
    format(Out, '~w="~w" ', [Name,Value]).

docker_comment(Out, Fmt, Args, docker) =>
    format(Out, Fmt, Args).
docker_comment(_Out, _Fmt, _Args, shell) =>
    true.

prepare_context(OS, Tag) :-
    os_dir(OS, Tag, Dir),
    directory_file_path(Dir, 'scripts.tgz', ScriptFile),
    process_create(path(tar),
                   [ '-C', 'share/scripts', '-zcf', ScriptFile, '.'],
                   []).

os_dir(OS, Tag, Dir) :-
    working_directory(WD, WD),
    atomic_list_concat([OS,Tag], /, File),
    directory_file_path(WD, File, Dir).

base_image(OS, Tag, Base) :-
    format(atom(Base), '~w-swipl:~w', [OS, Tag]).

copy_file_to(Out, File) :-
    absolute_file_name(File, AbsFile, [access(read)]),
    setup_call_cleanup(
        open(AbsFile, read, In),
        (   copy_stream_data(In, Out),
            nl(Out)
        ),
        close(In)).

docker_pull(Image, Tag) :-
    format(string(Img), '~w:~w', [Image, Tag]),
    process_create(path(docker),
                   [ pull, Img ],
                   []).

%!  docker_build(+Dockerfile, +Image) is det.
%
%   Run `docker build`, creating Image from   Dockerfile. Write a log to
%   Image.log

docker_build(Dockerfile, Image) :-
    file_directory_name(Dockerfile, Dir),
    file_base_name(Dockerfile, File),
    file_name_extension(Image, log, LogImage),
    directory_file_path(Dir, LogImage, LogFile),
    Argv = [ build, '-t', Image, '-f', File, '.' ],
    docker_run(Dir, Argv, LogFile).

docker_run(Dir, Argv, LogFile) :-
    process_create(path(docker), Argv,
                   [ stdout(pipe(Out)),
                     stderr(pipe(Error)),
                     process(PID),
                     cwd(Dir)
                   ]),
    thread_create(relay_output(LogFile, [output-Out, error-Error]), Id, []),
    process_wait(PID, Status),
    thread_join(Id, _),
    (   Status == exit(0)
    ->  true
    ;   throw(error(process_error(process(docker, Argv), Status), _))
    ).


relay_output(LogFile, Streams) :-
    setup_call_cleanup(
        open(LogFile, write, Log),
        relay_output_logged(Streams, Log),
        close(Log)).

relay_output_logged([], _) :- !.
relay_output_logged(Output, Log) :-
    pairs_values(Output, Streams),
    wait_for_input(Streams, Ready, infinite),
    relay(Ready, Output, NewOutputs, Log),
    relay_output_logged(NewOutputs, Log).

relay([], Outputs, Outputs, _).
relay([H|T], Outputs0, Outputs, Log) :-
    selectchk(Type-H, Outputs0, Outputs1),
    (   at_end_of_stream(H)
    ->  close(H),
        relay(T, Outputs1, Outputs, Log)
    ;   read_pending_codes(H, Codes, []),
        relay(Type, Codes, Log),
        relay(T, Outputs0, Outputs, Log)
    ).

relay(error,  Codes, Log) :-
    set_prolog_flag(message_context, []),
    print_error(Codes, []),
    format(Log, '~s', [Codes]),
    flush_output(Log).
relay(output, Codes, Log) :-
    phrase(status_output(Log), Codes, _),
    print_output(Codes, []),
    format(Log, '~s', [Codes]),
    flush_output(Log).

print_output(OutCodes, Options) :-
    option(output(Codes), Options),
    !,
    Codes = OutCodes.
print_output(OutCodes, _) :-
    print_message(informational, process_output(OutCodes)).

print_error(OutCodes, Options) :-
    option(error(Codes), Options),
    !,
    Codes = OutCodes.
print_error(OutCodes, _) :-
    phrase(classify_message(Level), OutCodes, _),
    print_message(Level, process_output(OutCodes)).

classify_message(error) -->
    string(_), "fatal:",
    !.
classify_message(error) -->
    string(_), "error:",
    !.
classify_message(warning) -->
    string(_), "warning:",
    !.
classify_message(informational) -->
    [].

string([]) --> [].
string([H|T]) --> [H], string(T).

%!  status_output(+Stream)//

:- thread_local
    benchmarks/0,
    bench_line/1.

status_output(Stream) -->
    { benchmarks },
    string(Codes), eol,
    { \+ phrase((string(_), "@@BENCH:"), Codes, _),
      !,
      string_codes(String, Codes),
      debug(bench, 'Got benchmark line ~p', [String]),
      assertz(bench_line(String))
    },
    status_output(Stream).
status_output(Stream) -->
    string(_), "@@", status_message(Stream), !, status_output(Stream).
status_output(_) -->
    "".

status_message(Stream) -->
    "START:", status_params(start, Dict, Stream),
    { broadcast(build(Dict)) }.
status_message(Stream) -->
    "PASSED:", status_params_cc(passed, Dict, Stream), msg_atom(Stage),
    { broadcast(build(Dict.put(stage,Stage))) }.
status_message(Stream) -->
    "FAILED:", status_params_cc(failed, Dict, Stream),
    { broadcast(build(Dict)) }.
status_message(_) -->
    "BENCH: begin", eol,
    { debug(bench, 'Starting capturing benchmark output', []),
      asserta(benchmarks)
    }.
status_message(Stream) -->
    "BENCH: end",
    msg_atom(Set),
    status_params_cc(bench, Dict, Stream),
    { retractall(benchmarks),
      debug(bench, 'Collecting benchmark set ~p', [Set]),
      findall(Line, retract(bench_line(Line)), Lines),
      atomics_to_string(Lines, "\n", Output),
      broadcast(build(Dict.put(_{set:Set, csv:Output})))
    }.
status_message(_Stream) -->
    string(Codes), "\n",
    { debug(status, 'Ignored: @@~s', [Codes]) }.

status_params(Event,
              _{file:File, event:Event, os:OS, tag:Tag, config:Config,
                time:Time, branch:Branch, version:Version},
              Stream) -->
    msg_atom(OS), msg_atom(Tag), msg_atom(Config),
    msg_time(Time), msg_atom(Branch), msg_version(Version),
    {   stream_property(Stream, file_name(File))
    ->  true
    ;   File = '-'
    }.

status_params_cc(Event, Dict, Stream) -->
    status_params(Event, Dict0, Stream),
    msg_atom(CC),
    { Dict = Dict0.put(cc, CC) }.


msg_atom(Atom)       --> whites, nonblanks(Codes), {atom_codes(Atom, Codes)}.
msg_time(Time)       --> whites, float(Time).
msg_version(Version) --> whites, nonblanks(Codes), {string_codes(Version, Codes)}.

eol --> "\r\n", !.
eol --> "\n".


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(process_output(Codes)) -->
    { split_lines(Codes, Lines) },
    process_lines(Lines).

split_lines([], []) :- !.
split_lines(All, [Line1|More]) :-
    append(Line1, [0'\n|Rest], All),
    !,
    split_lines(Rest, More).
split_lines(Line, [Line]).

process_lines([]) --> [].
process_lines([H|T]) -->
    [ '~s'-[H] ],
    (   {T==[]}
    ->  []
    ;   [nl], process_lines(T)
    ).
