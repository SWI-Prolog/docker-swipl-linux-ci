:- module(ci_server,
          []).
:- use_module(library(redis)).
:- use_module(library(redis_streams)).
:- use_module(ci).
:- use_module(library(broadcast)).
:- use_module(ci_redis).
:- use_module(library(debug)).
:- use_module(library(main)).

:- initialization(init_services, program).
:- initialization(main, main).

main(_Argv) :-
    thread_get_message(quit).

init_services :-
    thread_create(xlisten(ci, ['ci:request'], []), _,
                  [ alias(ci_requests)
                  ]),
    listen(redis(_Redis, 'ci:request', _Id, Data),
           catch_with_backtrace(ci(Data), E,
                                print_message(error, E))),
    listen(build(Status),
           store(Status)).

ci(Data) :-
    _{os:OS, tag:Tag} :< Data,
    (   Data.get(pull) == true
    ->  docker_pull_base_image(OS, Tag),
        test_base(OS, Tag, _)
    ;   test(OS, Tag,
             Data.get(type, incremental),
             Data.get(branch, master),
             Data.get(package_branches, _{}),
             Data.get(configs, [full]))
    ).

store(Dict) :-
    debug(event, 'Seding event ~p', [Dict]),
    redis(ci, publish(ci:event, prolog(Dict))),
    xadd(ci, ci:results, _Id, Dict).
