require.config({
  urlArgs: "ts="+new Date().getTime(),	/* prevent caching during development */
  waitSeconds: 60,			/* webstat-min.js is big */
  paths:
  { jquery:             "../node_modules/jquery/dist/jquery.min",
    laconic:            "../node_modules/laconic/laconic",
    bootstrap:          "../node_modules/bootstrap/dist/js/bootstrap.min",
    "tabulator-tables": "../node_modules/tabulator-tables/dist/js/tabulator.min"
  },
  shim:
  { bootstrap:
    { deps:["jquery"]
    },
    laconic:
    { deps:["jquery"]
    },
  }
}); //require.config

/*
 * Create the webstat application.
 */
require(["jquery", "ci", "laconic", "tabulator-tables"],
	function($, ci) {

function domain()
{ var os = [];
  var config = [];

  $("input.os[type=checkbox]").each(function() {
    var os_name = $(this).attr('id').replace("os:", "");

    os_name += "-" + $(this).closest("th").next().text();
    os.push(os_name);
  });
  $("input.config[type=checkbox]").each(function() {
    config.push($(this).attr('id').replace("config:", ""));
  });

  return {os:os, config:config};
}

function selection() {
  var os = [];
  var config = [];

  $("input.os[type=checkbox]:checked").each(function() {
    var os_name = $(this).attr('id').replace("os:", "");

    os_name += "-" + $(this).closest("th").next().text();
    os.push(os_name);
  });
  $("input.config[type=checkbox]:checked").each(function() {
    config.push($(this).attr('id').replace("config:", ""));
  });

  return {os:os, config:config};
}

function target_selection()
{ var dom = domain();
  var sel = selection();
  var targets = [];

  for(y=0; y<dom.os.length; y++)
  { var os = dom.os[y];
    var os_sel = sel.os.length == 0 ? true : sel.os.includes(os);

    for(x=0; x<dom.config.length; x++)
    { var cfg = dom.config[x];
      var is_sel;
      var id = os + "-" + cfg;

      if ( sel.config.length == 0 && sel.os.length == 0 )
	is_sel = false;
      else
	is_sel = os_sel && (sel.config.length == 0 ?
			      true : sel.config.includes(cfg));

      if ( is_sel )
      { targets.push(id);
	$("#"+id).addClass("selected");
      } else
	$("#"+id).removeClass("selected");
    }
  }

  return targets;			/* list of os-tag-config */
}

function target_osses(targets) {
  var osses = [];

  for(var i=0; i<targets.length; i++)
  { var osa = targets[i].split("-");

    osa.pop();
    var os = osa.join("-");
    if ( !osses.includes(os) )
      osses.push(os);
  }

  return osses;
}

function update_selection() {
  var targets = target_selection();
  var osses = target_osses(targets);

  if ( targets.length ) {
    $(".build-options").addClass("active");
    $(".build-button.config .build-label")
	.text("Build "+targets.length+" targets: ");
    $(".build-button.base .build-label")
	.text("Build "+osses.length+" targets: ");
    $(".build-button.config .build-targets").text(targets.join(", "));
    $(".build-button.base .build-targets").text(osses.join(", "));
    $(".build-status").text("");
  } else {
    $(".build-options").removeClass("active");
  }
}

function build_result(how, cls, message) {
  var span = $("div.build-button."+how+" .build-status");

  span.removeClass("text-success text-warning text-danger")
      .addClass("text-"+cls)
      .text(message);
}


function build(how) {
  var targets = target_selection();

  if ( how == "base" ) {
    var osses = target_osses(targets);
    $.get("/ci/build/"+how+"/"+osses.join("+"),
	  function(data) {
	    build_result(how, "success", "submitted");
	  })
      .fail(function() {
	build_result(how, "danger", "failed");
      });
  } else {
    var params = {branch: $("#remote").val() + "/" + $("#branch").val() };
    $.get("/ci/build/"+how+"/"+targets.join("+"),
	  params,
	  function(data) {
	    build_result(how, "success", "submitted");
	  })
      .fail(function() {
	build_result(how, "danger", "failed");
      });
  }
}

function event(data) {
  if ( window.build_table )
    window.build_table.addData(data, true);
}

function build_events() {
  $.get("/ci/event", function(data) {
    if ( data )
      event(data);
    build_events();
  }).fail(build_events);
}

function fill_branches(remote) {
  $.get("/ci/branches/"+remote, function(data) {
    $("#branch").html("");
    for(var i=0; i<data.length; i++) {
      $("#branch").append('<option value="'+data[i]+'">'+data[i]+'</option>');
    }
    if ( data.includes("master") )
      $("#branch").val("master");
  });
}


$("input.os[type=checkbox]").on('change', update_selection);
$("input.config[type=checkbox]").on('change', update_selection);
$("div.build-button.incremental button").click(function() {
  build("incremental");
});
$("div.build-button.clean button").click(function() {
  build("clean");
});
$("div.build-button.base button").click(function() {
  build("base");
});
$("#remote").on('change', function(ev) {
  var remote = $(ev.target).val();
  fill_branches(remote);
});

build_events();
update_selection();
fill_branches($("#remote").val());
});
