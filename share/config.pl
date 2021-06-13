:- module(ci_config,
          [ remote/2,
            redis_config/2
          ]).
:- use_module(library(yaml)).
:- use_module(library(lists)).

remote(Name, URL) :-
    yaml_read('config.yaml', DOM),
    member(Rm, DOM.remotes),
    remote(Rm, Name,URL).

remote(Dict, Name, URL) :-
    dict_pairs(Dict, _, [Name-Attrs]),
    URL = Attrs.url.

redis_config(Host:Port, Options) :-
    yaml_read('config.yaml', DOM),
    atom_string(Host, DOM.redis.get(host, localhost)),
    Port = DOM.redis.get(port, localhost),
    (   _{ user:User, password: Password } :< DOM.redis
    ->  Options = [user(User), password(Password), version(3)]
    ;   _{ password: Password } :< DOM.redis
    ->  Options = [password(Password)]
    ;   Options = []
    ).
