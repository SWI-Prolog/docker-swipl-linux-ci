:- module(ci_server,
          []).
:- use_module(library(redis)).
:- use_module(library(redis_streams)).
:- use_module(ci).
:- use_module(library(broadcast)).
:- use_module(ci_redis).

:- thread_create(xlisten(ci, ['ci:request'], []), _,
                 [ alias(ci_requests)
                 ]).
:- listen(redis(_Redis, 'ci:request', _Id, Data),
          ci(Data)).
:- listen(build(Status),
          store(Status)).

ci(Data) :-
    _{os:OS, tag:Tag} :< Data,
    (   Data.get(pull) == true
    ->  docker_pull(OS, Tag),
        test_base(OS, Tag, _)
    ;   test(OS, Tag,
             Data.get(type, incremental),
             Data.get(branch, master),
             Data.get(package_branches, _{}),
             Data.get(configs, [full]))
    ).

store(Dict) :-
    xadd(ci, ci:results, _Id, Dict).
