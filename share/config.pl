:- module(ci_config,
          [remote/2]).
:- use_module(library(yaml)).
:- use_module(library(lists)).

remote(Name, URL) :-
    yaml_read('config.yaml', DOM),
    member(Rm, DOM.remotes),
    remote(Rm, Name,URL).

remote(Dict, Name, URL) :-
    dict_pairs(Dict, _, [Name-Attrs]),
    URL = Attrs.url.
