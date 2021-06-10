:- module(web_resources,
          [ ci_logo//1,
            ci_resources//0,
            glyph//1
          ]).
:- use_module(library(debug)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).

:- multifile
    user:file_search_path/2.

i_am_here.

user:file_search_path(ci_web, Dir) :-
    source_file(web_resources:i_am_here, File),
    file_directory_name(File, Dir).
user:file_search_path(ci_web_web,          ci_web(web)).
user:file_search_path(ci_web_css,          ci_web_web(css)).
user:file_search_path(ci_web_js,           ci_web_web(js)).
user:file_search_path(ci_web_icons,        ci_web_web(icons)).
user:file_search_path(ci_web_node_modules, ci_web(node_modules)).

http:location(ci,            root(ci), []).
http:location(ci_css,        ci(css), []).

:- http_handler(ci(css),
                serve_files_in_directory(ci_web_css), [prefix]).
:- http_handler(ci(js),
                serve_files_in_directory(ci_web_js), [prefix]).
:- http_handler(ci(icons),
                serve_files_in_directory(ci_web_icons), [prefix]).
:- http_handler(ci(node_modules),
                serve_files_in_directory(ci_web_node_modules), [prefix]).
:- http_handler(root('favicon.ico'), favicon,
		[priority(10)]).
:- http_handler(root('apple-touch-icon.png'), touch_icon, []).

ci_logo(_Options) -->
    { http_absolute_location(ci(.), HREF, [])
    },
    html(a([href(HREF), class(['ci-logo', 'navbar-brand'])], &(nbsp))).

ci_resources -->
    ci_css,
    ci_js.

ci_js  --> html_post(head, \include_ci_js).
ci_css --> html_post(head, \include_ci_css).

include_ci_js -->
	{ ci_resource(js, JS),
	  ci_resource(rjs, RJS),
	  http_absolute_location(ci(js/JS), WebStatJS, []),
	  http_absolute_location(ci(RJS),   WebStatRJS, [])
	},
	rjs_timeout(JS),
	html(script([ src(WebStatRJS),
		      'data-main'(WebStatJS)
		    ], [])).

rjs_timeout('ci-min') --> !,
	js_script({|javascript||
// Override RequireJS timeout, until main file is loaded.
window.require = { waitSeconds: 0 };
		  |}).
rjs_timeout(_) --> [].


include_ci_css -->
	{ ci_resource(css, CSS),
	  http_absolute_location(ci(css/CSS), WebStatCSS, [])
	},
	html(link([ rel(stylesheet),
		    href(WebStatCSS)
		  ])).

ci_resource(Type, ID) :-
	alt(Type, ID, File),
	(   File == (-)
	;   absolute_file_name(File, _P, [file_errors(fail), access(read)])
	), !.

alt(js,  'ci-min',     ci_web_js('ci-min.js')) :-
	\+ debugging(nominified).
alt(js,  'ci',         ci_web_js('ci.js')).
alt(css, 'ci-min.css', ci_web_css('ci-min.css')) :-
	\+ debugging(nominified).
alt(css, 'ci.css',     ci_web_css('ci.css')).
alt(rjs, 'js/require.js', ci_web_js('require.js')) :-
	\+ debugging(nominified).
alt(rjs, 'node_modules/requirejs/require.js', -).

%%	favicon(+Request)
%
%	Serve /favicon.ico.

favicon(Request) :-
	http_reply_file(ci_web_icons('favicon.ico'), [], Request).

%%	touch_icon(+Request)
%
%	Serve /apple-touch-icon.png.

touch_icon(Request) :-
	http_reply_file(ci_web_icons('apple-touch-icon.png'), [], Request).

glyph(Name) -->
    { atom_concat('glyphicon-', Name, GlName)
    },
    html(span([class([glyphicon, GlName]), 'aria-hidden'(true)],[])).
