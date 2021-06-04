:- module(ci,
          [ test_base/3,     % +OS,+Tag,-Base
            test/6	     % +OS,+Tag,+Type,+Branch,+PackageBranches,+Configs
          ]).
:- autoload(library(apply), [maplist/2, maplist/3]).
:- autoload(library(dicts), [dict_keys/2]).
:- autoload(library(filesex), [directory_file_path/3]).
:- autoload(library(lists), [selectchk/3, append/2, append/3]).
:- autoload(library(option), [option/2]).
:- autoload(library(pairs), [pairs_values/2]).
:- autoload(library(process), [process_create/3, process_wait/2]).
:- autoload(library(yaml), [yaml_read/2]).

:- multifile
    user:file_search_path/2.

user:file_search_path(share, share).

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
    copy_file_to(Out, share('Download.docker')),
    config_dict(Dir, Config),
    maplist(docker_config(Out, Config.default, _, _), Config.configs),
    copy_file_to(Out, share('Scripts.docker')).

config_dict(Dir, Config) :-
    directory_file_path(Dir, 'ci.yaml', CI),
    yaml_read(CI, Config).

:- meta_predicate
    if_step(+,+,0).

docker_config(Out, Defaults, Steps, Configs, ConfigDict) :-
    dict_keys(ConfigDict, [Config]),
    memberchk(Config, Configs),
    !,
    atom_concat('build.', Config, BuildDir),
    config_options(Defaults, ConfigDict.Config, Options),

    Configure = [ [ rm, '-rf', BuildDir ],
                  [ mkdir, BuildDir],
                  [ cd, BuildDir ],
                  [ cmake, Options.cmake_flags,
                           '-G', Options.generator.cmake,
                           '..'
                  ] - Options.cmake_env
                ],

    Build =     [ [ cd, BuildDir ],
                  [ Options.generator.command ]
                ],

    Test =	[ [ cd, BuildDir ],
                  [ ctest, '-j', '$(nproc)', '--output-on-failure'
                  ] - Options.ctest_env
                ],

    format(Out, '~N~n# Configuration ~w: ~s~n', [Config, Options.comment]),
    if_step(configure, Steps, docker_command(Out, Configure)),
    if_step(build,     Steps, docker_command(Out, Build)),
    if_step(test,      Steps, docker_command(Out, Test)).
docker_config(_Out, _Defaults, _Steps, _Configs, _ConfigDict).

if_step(Step, Steps, Command) :-
    (   memberchk(Step, Steps)
    ->  call(Command)
    ;   true
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

test(OS, Tag, Type, Branch, PackageBranches, Configs) :-
    must_be(oneof([clean,incremental]), Type),
    test_docker_file(OS, Tag, Type, Branch, PackageBranches, Configs,
                     Dockerfile, Image),
    docker_build(Dockerfile, Image),
    true.

test_docker_file(OS, Tag, Type, Branch, PackageBranches, Configs,
                 Dockerfile, Image) :-
    test_id(Type, Branch, PackageBranches, TestID),
    format(atom(Image), '~w-~w-~w', [OS,Tag,TestID]),
    format(atom(DockerBase), 'Dockerfile.~w', [TestID]),
    os_dir(OS, Tag, Dir),
    directory_file_path(Dir, DockerBase, Dockerfile),
    setup_call_cleanup(
        open(Dockerfile, write, Out),
        test_docker_file(Out, OS, Tag, Type, Branch, PackageBranches, Configs),
        close(Out)).

test_id(Type, Branch, PackageBranches, IncrImage) :-
    branch_id(Branch, PackageBranches, BranchID),
    format(atom(IncrImage), '~w-~w', [BranchID, Type]).

branch_id(Branch, PackageBranches, Id) :-
    dict_pairs(PackageBranches, _, Pairs),
    maplist(pkg_branch_id, Pairs, PkgIds),
    atomics_to_string([Branch|PkgIds], -, Id).

pkg_branch_id(Pkg-Branch, Id) :-
    format(string(Id), '~w-~w', [Pkg, Branch]).

test_docker_file(Out, OS, Tag, Type, Branch, PackageBranches, Configs) :-
    base_image(OS, Tag, Base),
    copy_file_to(Out, share('Header.docker')),
    format(Out, 'FROM ~w~n~n', [Base]),
    checkout_version_command(Out, Branch, PackageBranches),
    os_dir(OS, Tag, Dir),
    config_dict(Dir, Config),
    type_steps(Type, Steps),
    maplist(docker_config(Out, Config.default,
                          Steps,
                          Configs), Config.configs).

type_steps(incremental, [build, test]).
type_steps(clean,       [configure,build, test]).

checkout_version_command(Out, Branch, PackageBranches) :-
    dict_pairs(PackageBranches, _, Pairs),
    maplist(pkg_checkout_cmd, Pairs, Cmds),
    append(Cmds, PkgsCmd),
    docker_command(Out,
                   [ [ '/opt/bin/swipl-checkout',
                       '-b', Branch
                     | PkgsCmd
                     ]
                   ]).

pkg_checkout_cmd(Pkg-Branch, ['-p', Pkg, Branch]).


		 /*******************************
		 *           PROCESSES		*
		 *******************************/

%!  docker_command(+Out, +Command)

docker_command(Out, Command) :-
    format(Out, '~N~nRUN\t', []),
    docker_command_lines(Out, Command).

docker_command_lines(_, []) :-
    !.
docker_command_lines(Out, [H|T]) :-
    docker_command_line(Out, H),
    (   T == []
    ->  nl(Out)
    ;   format(Out, ' && \\~n\t', []),
        docker_command_lines(Out, T)
    ).

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

shell_quote(Arg, QArg) :-
    split_string(Arg, " ()[]?", "", [_,_|_]),
    !,
    format(string(QArg), '"~w"', Arg).
shell_quote(Arg, Arg).

env_command(Out, Name-Value) :-
    format(Out, '~w="~w" ', [Name,Value]).

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

docker_build(Dockerfile, Base) :-
    file_directory_name(Dockerfile, Dir),
    file_base_name(Dockerfile, File),
    file_name_extension(Base, log, LogBase),
    directory_file_path(Dir, LogBase, LogFile),
    Argv = [ build, '-t', Base, '-f', File, '.' ],
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
