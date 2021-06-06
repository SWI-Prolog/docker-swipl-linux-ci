:- module(web_server,
          [ server/1
          ]).
:- use_module(library(http/http_server)).
:- use_module(library(http/html_write)).
:- use_module(library(dcg/high_order)).
:- use_module(ci).
:- use_module(ci_redis).
:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(lists)).
:- use_module(library(pure_input)).
:- use_module(library(redis)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).

server(Port) :-
    http_server([ port(Port)
                ]).

:- http_handler(root(.), http_redirect(see_other, root(home)), []).
:- http_handler(root(home),      home,     []).
:- http_handler(root(view_log),  view_log, []).

home(_Request) :-
    reply_html_page(title('SWI-Prolog Continuous integration status'),
                    \home_page).

home_page -->
    html(h1('SWI-Prolog Continuous integration status')),
    summary_table,
    build_table.

%!  summary_table//
%
%   Lists known OS/Tag and configurations

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

		 /*******************************
		 *            BUILDS		*
		 *******************************/

build_table -->
    { builds(Dicts),
      reverse(Dicts, Builds)
    },
    build_table(Builds).

builds(Dicts) :-
    redis(ci, xrange(ci:results, -, +), Builds),
    maplist(build_dict, Builds, Dicts).

build_dict([_Time,Array], Dict) :-
    redis_array_dict(Array, build, Dict).

build_table(Dicts) -->
    html(table(class(builds),
               \sequence(build_event, Dicts))).

build_event(Dict) -->
    build_event(Dict.event, Dict).

build_event(passed, Dict) -->
    !,
    html(tr([ td(Dict.event),
              td(Dict.os),
              td(Dict.tag),
              td(Dict.cc),
              td(Dict.config),
              td(Dict.version),
              td(Dict.stage),
              td(\completed(Dict.time)),
              td(\log(Dict))
            ])).
build_event(failed, Dict) -->
    !,
    html(tr([ td(Dict.event),
              td(Dict.os),
              td(Dict.tag),
              td(Dict.cc),
              td(Dict.config),
              td(Dict.version),
              td([]),
              td(\completed(Dict.time)),
              td(\log(Dict))
            ])).
build_event(bench, Dict) -->
    !,
    html(tr([ td(Dict.event),
              td(Dict.os),
              td(Dict.tag),
              td(Dict.cc),
              td(Dict.config),
              td(Dict.version),
              td([]),
              td(\completed(Dict.time)),
              td(\bench_rating(Dict.csv))
            ])).
build_event(_, _) -->
    [].

completed(Time) -->
    { format_time(string(S), '%F %T', Time) },
    html([S]).

bench_rating(CSV) -->
    { setup_call_cleanup(
          open_string(CSV, In),
          csv_read_stream(In, Rows, []),
          close(In)),
      member(row(average, Time, _Gc), Rows)
    },
    html(Time).

log(Dict) -->
    { working_directory(Dir,Dir),
      directory_file_path(Dir, rel, RelTo),
      relative_file_name(Dict.file, RelTo, RelFile),
      http_link_to_id(view_log, [file(RelFile), time(Dict.time)], HREF)
    },
    html(a(href=HREF, "View log")).

%!  view_log(+Request)
%
%   View a log file. Extracts the relevant   fragment  of the entire log
%   using the time stamp as identifier for the @@-line.

view_log(Request) :-
    http_parameters(Request,
                    [ file(File, []),
                      time(Stamp, [float])
                    ]),
    safe_file(File),
    debug(log, 'Looking for = ~p', [Stamp]),
    phrase_from_file(extract_log(Stamp, Data), File),
    reply_html_page(title('SWI-Prolog build log'),
                    pre(Data)).

safe_file(File) :-
    \+ is_absolute_file_name(File),
    \+ sub_atom(File, 0, _, _, '../'),
    \+ sub_atom(File, _, _, _, '/../'),
    !.
safe_file(File) :-
    permission_error(access, file, File).

extract_log(Stamp, Data) -->
    string(_), "@@", (string(_), "\n" -> []), string(Codes), 'peek@@',
    !,
    (   string_no_nl, white, float(F),
        { debug(log, 'Stamp = ~p', [F]),
          Stamp =:= F,
          debug(log, 'Found!', [])
        }
    ->  { string_codes(Data, Codes) },
        remainder(_)
    ;   extract_log(Stamp, Data)
    ).

'peek@@', "@@" --> "@@".

string_no_nl -->
    "".
string_no_nl -->
    [C],
    { C \== 0'\n },
    string_no_nl.
