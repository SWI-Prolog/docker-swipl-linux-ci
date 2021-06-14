:- module(ci_web,
          [ server/1
          ]).
:- use_module(library(http/http_server)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_authenticate), []).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(dcg/high_order)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pure_input)).
:- use_module(library(redis)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(option)).
:- use_module(library(broadcast)).
:- use_module(library(git)).
:- use_module(library(readutil)).

:- use_module(ci).
:- use_module(ci_client).
:- use_module(web_resources).
:- use_module(config).

server(Port) :-
    http_server([ port(Port)
                ]).

:- http_handler(root(.),
                http_redirect(see_other, ci(home)), []).
:- http_handler(ci(home),              home(guest),        []).
:- http_handler(ci(dev),               home(dev),
                [ authentication(basic(passwd, 'Developer'))
                ]).
:- http_handler(ci(view_log),          view_log,           []).
:- http_handler(ci(build),             http_404([]),
                [ authentication(basic(passwd, 'Developer')),
                  prefix
                ]).
:- http_handler(ci(build/incremental), build(incremental), [ prefix]).
:- http_handler(ci(build/clean),       build(clean),       [ prefix]).
:- http_handler(ci(build/base),        build(base),        [ prefix]).
:- http_handler(ci(event),             build_event,        []).
:- http_handler(ci(branches),          remote_branches,    [ prefix ]).
:- http_handler(ci('recent-builds'),   recent_builds,      []).

home(Role, _Request) :-
    reply_html_page(title('SWI-Prolog Continuous integration status'),
                    \home_page([role(Role)])).

home_page(Options) -->
    ci_resources,
    navbar(Options),
    html(div(class([content,container]), \content(Options))).

navbar(Options) -->
    html(nav([ class([navbar, 'navbar-default']),
               role(navigation)
             ],
             [ div(class('navbar-header'),
                   [ \collapsed_button,
                     \ci_logo(Options)
                   ]),
               div([ class([collapse, 'navbar-collapse']),
                     id(navbar)
                   ],
                   [ ul([class([nav, 'navbar-nav', menubar])], []),
                     ul([class([nav, 'navbar-nav', 'navbar-right'])], [])
                   ])
             ])).

collapsed_button -->
    html(button([type(button),
                 class('navbar-toggle'),
                 'data-toggle'(collapse),
                 'data-target'('#navbar')
                ],
                [ span(class('sr-only'), 'Toggle navigation'),
                  span(class('icon-bar'), []),
                  span(class('icon-bar'), []),
                  span(class('icon-bar'), [])
                ])).

content(Options) -->
    html(h1('SWI-Prolog Continuous integration status')),
    { builds(Dicts),
      reverse(Dicts, Builds)
    },
    summary_table(Builds, Options),
    build_table(Builds).

%!  summary_table(+Builds, +Options)//
%
%   Lists known OS/Tag and configurations

summary_table(Builds, Options) -->
    { findall(c(OS,Tag,Configs), current_test(OS, Tag, Configs), Triples),
      maplist(arg(3), Triples, CLists),
      append(CLists, AllConfigs),
      sort(AllConfigs, CCols),
      maplist(arg(1), Triples, OS0),
      sort(OS0, OS)
    },
    html(table(class([table, results, 'table-condensed', 'table-bordered']),
               [ tr([th('OS'),th('Version')|\sequence(th_config(Options), CCols)])
               | \sequence(os(Triples, CCols, Builds, Options), OS)
               ])),
    html(div(class('build-options'),
             \update_buttons(Options))).

update_buttons(Options) -->
    { option(role(dev), Options) },
    !,
    select_remote,
    update_button(incremental, config,
                  'btn-primary',
                  'Incremental build'),
    update_button(clean, config,
                  'btn-secondary',
                  'Clean build'),
    update_button(base, base,
                  'btn-warning',
                  'Update OS and base image'),
    html(div(id('build-events'), [])).
update_buttons(_) -->
    [].

select_remote -->
    { findall(R, remote(R,_), Rs) },
    html([ div(class('form-group'),
               [ label(for(remote), 'From remote'),
                 select([class('form-control'), id(remote)],
                        \sequence(remote_option, Rs))
               ]),
           div(class('form-group'),
               [ label(for(branch), 'Using branch'),
                 select([class('form-control'), id(branch)],
                        [])
               ])
         ]).

remote_option(Remote) -->
    html(option(value(Remote), Remote)).


update_button(Type, Rebuild, BtnType, Label) -->
    html(div(class(['build-button', Type, Rebuild]),
             [ button([type(button), class([btn, BtnType])], Label),
               span([class('build-label')], []),
               span([class('build-targets')], []),
               span([class('build-status')], [])
             ])).

th_config(Options, Config) -->
    html(th([\check('config:', config, Config, Options), ' ', Config])).

os(Triples, AllConfigs, Builds, Options, OS) -->
    foreach(member(c(OS,Tag,Configs), Triples),
            html(tr([ th([\check('os:', os, OS, Options), ' ', OS]), th(Tag)
                    | \sequence(config_cell(OS,Tag,Configs,Builds), AllConfigs)
                    ]))).

config_cell(OS, Tag, Configs, Builds, Config) -->
    { memberchk(Config, Configs),
      atomic_list_concat([OS,Tag,Config], -, ID)
    },
    !,
    (   {pass(OS, Tag, Config, [master, 'origin-master'], Builds)}
    ->  html(td([class([status,pass]), id(ID)], \glyph(ok)))
    ;   html(td([class([status,fail]), id(ID)], \glyph(remove)))
    ).
config_cell(_OS, _Tag, _AllConfigs, _Config, _Builds) -->
    html(td(title('Not available'), -)).

pass(OS, Tag, Config, Branches, Builds) :-
    member(B, Builds),
    _{os:OS, tag:Tag2, config:Config, event: Event, stage:Stage, branch:Branch} :< B,
    memberchk(Branch, Branches),
    same_tag(Tag, Tag2),
    (   Stage == failed
    ->  !, fail
    ;   Stage == test,
        Event == passed
    ->  !
    ;   Stage == build,
        Event == passed,
        config_dict(OS, Tag, Config, Options),
        Options.get(test) == false
    ).

same_tag(T, T) :-
    !.
same_tag(T1, T2) :-
    atom(T1), number(T2),
    atom_number(T1, T2).

check(Prefix, Class, Id, Options) -->
    { option(role(dev), Options),
      !,
      atom_concat(Prefix, Id, TheID)
    },
    html(input([ type(checkbox),
                 class(['form-check-input', Class]),
                 id(TheID)
               ])).
check(_, _, _, _) -->
    [].


		 /*******************************
		 *            BUILDS		*
		 *******************************/

build_table(_Builds) -->
    html(h2('Recent builds')),
    html(div(id('recent-build-table'), [])),
    html({|html||
<script>
require(["../node_modules/tabulator-tables/dist/js/tabulator.min"],
        function(Tabulator) {

function bench_average(csv) {
  if ( csv ) {
    var lines = csv.split("\n");
    var last = lines[lines.length-1];
    var cols = last.split(",");
    return cols[1];
  } else {
    return "";
  }
}

function cmp_bench(a, b, dir) {
  var pa = bench_average(a);
  var pb = bench_average(b);
  var md = dir == "asc" ? 1 : -1;

  if ( pa == pb )
    return 0;
  else if ( pa && pb )
    return Number(pa)-Number(pb);
  else if ( pa )
    return -1*md;
  else
    return 1*md;
}

function event_glyph(ev, stage) {
  return {start:  {glyph:"forward", color:"brown"},
          passed: [ {glyph:"ok",    color:"LimeGreen"},
                    {configure: { glyph:"cog",   color:"LimeGreen" },
                     build:     { glyph:"flash", color:"LimeGreen" },
                     test:      { glyph:"flag",  color:"LimeGreen" }
                    }[stage] || {glyph:"question-sign", color:"red"}
                  ],
          failed: {glyph:"remove",  color:"red"},
          bench:  {glyph:"music",   color:"blue"}
         }[ev]
               || {glyph:"question-sign", color:"red"};
}

function glyph(obj) {
  return '<span style="color:'+obj.color+
             '" class="glyphicon glyphicon-'+obj.glyph+
             '"></span>';
}


var table = new Tabulator("#recent-build-table", {
  ajaxURL:"/ci/recent-builds",
  layout:"fitDataStretch",
  layoutColumnsOnNewData:true,
  columns: [
    {title: "Event",      field: "event",   sorter:"string", hozAlign:"center",
     formatter: function(cell, params, onrendered) {
       var data = cell.getRow().getData();
       var icon = event_glyph(data.event, data.stage);
       if ( Array.isArray(icon) ) {
         var res = "";
         for(var i=0; i<icon.length; i++) {
           res += glyph(icon[i]);
         }
         return res;
       } else {
         return glyph(icon);
       }
     },
     tooltip: function(cell) {
       var data = cell.getRow().getData();
       return data.event + " " + (data.stage || "");
     }
    },
    {title: "OS",         field: "os",      sorter:"string"},
    {title: "OS Tag",     field: "tag",     sorter:"number", hozAlign:"center"},
    {title: "CC",         field: "cc",      sorter:"string"},
    {title: "Config",     field: "config",  sorter:"string", hozAlign:"center"},
    {title: "Branch",     field: "branch",  sorter:"string",
     formatter: function(cell, params, onrendered) {
       var branch = cell.getValue().replace(/([^-]*)-/, "$1/")
                        .replace("origin/", "");
       return '<span class="branch-'+branch+'">'+branch+'</span>'
     }
    },
    {title: "Version",    field: "version", sorter:"string"},
    {title: "Completed",  field: "time",
     formatter: function(cell, params, onrendered) {
       var date = new Date(cell.getValue()*1000);
       return date.toLocaleString();
     }
    },
    {title: "Benchmark",  field: "csv",
     formatter: function(cell, params, onrendered) {
       return bench_average(cell.getValue());
     },
     sorter: function(a, b, aRow, bRow, column, dir, sorterParams) {
       return cmp_bench(a,b,dir);
     }
    },
    {title: "Log",
     formatter: function(cell, formatterParams, onRendered) {
       return '<a class="view-log">View log</a>';
     },
     cellClick: function(e, cell) {
       data = cell.getRow().getData();

       window.location = "/ci/view_log?file="+encodeURIComponent(data.file)
                                             +"&time="+data.time;
     }
    }
  ]
});

window.build_table = table;

});

</script>
         |}).

recent_builds(_Request) :-
    builds(Dicts),
    reverse(Dicts, Builds),
    reply_json_dict(Builds).

builds(Dicts) :-
    redis(ci, xrange(ci:results, -, +), Builds),
    maplist(build_dict, Builds, Dicts).

build_dict([_Time,Array], Dict) :-
    redis_array_dict(Array, build, Dict0),
    fixup_event_dict(Dict0, Dict).

fixup_event_dict(Dict0, Dict) :-
    relative_file(Dict0.file, RelFile),
    Dict = Dict0.put(file,RelFile).

relative_file(File, RelFile) :-
    working_directory(Dir,Dir),
    directory_file_path(Dir, rel, RelTo),
    relative_file_name(File, RelTo, RelFile).

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
    extract_log(File, Stamp, Data),
    reply_html_page(title('SWI-Prolog build log'),
                    pre(Data)).

safe_file(File) :-
    \+ is_absolute_file_name(File),
    \+ sub_atom(File, 0, _, _, '../'),
    \+ sub_atom(File, _, _, _, '/../'),
    !.
safe_file(File) :-
    permission_error(access, file, File).

extract_log(File, Stamp, Data) :-
    phrase_from_file(extract_log(Stamp, Data), File),
    !.
extract_log(File, Stamp, Data) :-
    phrase_from_file(extract_log_start(Stamp, Data), File),
    !.
extract_log(File, _, Data) :-
    read_file_to_string(File, Data, []).

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

extract_log_start(Stamp, Data) -->
    string(Codes), "@@",
    !,
    (   string_no_nl, white, float(F),
        { debug(log, 'Stamp = ~p', [F]),
          Stamp =:= F,
          debug(log, 'Found!', [])
        }
    ->  { string_codes(Data, Codes) },
        remainder(_)
    ).


'peek@@', "@@" --> "@@".

string_no_nl -->
    "".
string_no_nl -->
    [C],
    { C \== 0'\n },
    string_no_nl.


		 /*******************************
		 *            COMMANDS		*
		 *******************************/

%!  build(+How, +Request)
%
%   Handle a build request.  The  "file   name"  contains  the requested
%   targets separated by `+`. How is   one  of `incremental`, `clean` or
%   `base`.

build(How, Request) :-
    http_parameters(Request,
                    [ branch(Branch, [default(master)])
                    ]),
    option(path_info(Info), Request),
    debug(build, 'Path info: ~p', [Info]),
    string_concat("/", Path, Info),
    split_string(Path, "+", "", Configs),
    maplist(submit_test(How, Branch), Configs),
    reply_json_dict(true).

submit_test(How, Branch, Spec) :-
    split_string(Spec, "-", "", Parts),
    maplist(untyped, Parts, [OS,Tag,Config]),
    !,
    request_test(_{type:How, os:OS, tag:Tag, branch:Branch, configs:[Config]}).
submit_test(base, _Branch, Spec) :-
    split_string(Spec, "-", "", Parts),
    maplist(untyped, Parts, [OS,Tag]),
    !,
    request_test(_{os:OS, tag:Tag, pull:true}).

untyped(S, N) :-
    number_string(N, S),
    !.
untyped(S, A) :-
    atom_string(A, S).

remote_branches(Request) :-
    option(path_info(Info), Request),
    atom_concat("/", Remote, Info),
    remote(Remote, URL),
    git_remote_branches(URL, Branches),
    reply_json_dict(Branches).


		 /*******************************
		 *             EVENTS		*
		 *******************************/

:- initialization(relay_events, program).

relay_events :-
    message_queue_create(_, [alias(build_events)]),
    redis_subscribe(ci, [ci:event], _Id, [alias(events)]),
    listen(redis(ci, 'ci:event', Data),
           thread_send_message(build_events, msg(Data))).

build_event(_Request) :-
    (   thread_get_message(build_events, msg(Msg0), [timeout(60)])
    ->  debug(event, 'Got event ~p', [Msg0]),
        fixup_event_dict(Msg0, Msg),
        reply_json_dict(Msg)
    ;   reply_json_dict(null)
    ).

