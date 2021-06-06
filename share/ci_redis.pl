:- module(ci_redis,
          []).
:- use_module(library(redis)).
:- use_module(library(redis_streams)).

:- initialization redis_server(ci, localhost:6379, [ /*version(3)*/ ]).
%:- initialization make_group(ci, ci:request, 100).

/* <module> CI Redis commons

Redis key usage:

  - ci:request
    Stream to send new requests
  - ci:results
    Stream holding build results
*/

make_group(Group, Key, MaxLen) :-
    add_consumer_group(Group, Key),
    xstream_set(swish, Key, maxlen(MaxLen)).

add_consumer_group(-, _) :-
    !.
add_consumer_group(Group, Key) :-
    catch(redis(swish, xgroup(create, Key, Group, $, mkstream), _),
          error(redis_error(busygroup,_),_),
          true).
