:- module(web_server,
          [ server/1
          ]).
:- use_module(library(http/http_server)).
:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- use_module(ci).

server(Port) :-
    http_server([ port(Port)
                ]).

:- http_handler(root(.), http_redirect(see_other, root(home)), []).
:- http_handler(root(home), home, []).

home(_Request) :-
    reply_html_page(title('SWI-Prolog Continuous integration status'),
                    [ h1('SWI-Prolog Continuous integration status')
                    | \summary_table
                    ]).

summary_table -->
    { findall(c(OS,Tag,Configs), current_test(OS, Tag, Configs), Triples),
      maplist(arg(3), Triples, CLists),
      append(CLists, AllConfigs),
      sort(AllConfigs, CCols),
      maplist(arg(1), Triples, OS0),
      sort(OS0, OS)
    },
    html(table(class(results),
               [ tr([th('OS'),th('Version')|\sequence(th_config, CCols)])
               | \sequence(os(Triples, CCols), OS)
               ])).

th_config(Config) -->
    html(th(Config)).

os(Triples, AllConfigs, OS) -->
    foreach(member(c(OS,Tag,Configs), Triples),
            html(tr([th(OS), th(Tag) | \sequence(config_cell(OS,Tag,AllConfigs), Configs)]))).

config_cell(_OS, _Tag, AllConfigs, Config) -->
    { memberchk(Config, AllConfigs) },
    !,
    html(td(+)).
config_cell(_OS, _Tag, _AllConfigs, _Config) -->
    html(td(-)).

