:- module(ci_client,
          [ request_test/1 ]).
:- use_module(library(redis)).
:- use_module(library(redis_streams)).
:- use_module(ci_redis).

%!  request_test(+Dict) is det.
%
%   Execute a test.  Dict contains:
%
%     - os:OS
%     - tag:Tag
%     - type:Type
%     - branch:Branch
%     - package_branches:Dict
%     - configs:list
%     - pull:boolean
%       If present and `true`, pull the base image and start from
%       scratch

request_test(Dict0) :-
    dict_pairs(Dict0, Tag, Pairs0),
    maplist(to_prolog, Pairs0, Pairs),
    dict_pairs(Dict, Tag, Pairs),
    xadd(ci, ci:request, _Id, Dict).

to_prolog(Key-Value, Key-prolog(Value)).
