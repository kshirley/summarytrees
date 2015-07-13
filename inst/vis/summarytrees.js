// set up some global variables
var margin = {top: 50, right: 50, bottom: 50, left: 50};
var width = 1000;
var height = 1200;
var basetree;
var basedata;
var root;
var old_tree; 
var current_tree;
var new_location;
var fixed_depth = 200;
var current_k;
var old_k = 1; 
var min_k = 1; 
var max_k;

// To display the value of k in the box on the webpage:
function set_k(new_k) {
  current_k = new_k;
  document.getElementById("newk").value = current_k;
}

// Set the divisor to control width (note: figure out what I was doing here)
var divisor;
d3.json("divisor.json", function(error, json) {
  divisor = json;
  // set K (called 'max_k' here):
  max_k = divisor.length;
  // k = 20 is the default initial value (restricted to be between 1 and K):
  set_k(Math.max(min_k, Math.min(max_k, 20)));    
});

var maxPrint;

var vis = d3.select("#summarytree-drawing").append("svg:svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("svg:g")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// the dark gray rectangle defining the data plotting area:
vis.append("rect")
  .attr("x", 0)
  .attr("y", 0)
  .attr("width", width)
  .attr("height", height)
  .attr("fill", "black")
  .attr("fill-opacity", 0.10)
  .attr("stroke", "black")
  .attr("stroke-opacity", 0.00);

// the light gray rectangle defining the total plotting area
// This includes room around the data plotting area for margin labels, etc.
vis.append("rect")
  .attr("x", -margin.left)
  .attr("y", -margin.top)
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .attr("fill", "black")
  .attr("fill-opacity", 0.05);

var tree = d3.layout.tree()
  .size([height - 20, width])
//.attr("transform", "translate(" + 20 + "," + 0 + ")");
//.separation(function(a, b) { return (a.parent == b.parent ? 1 : 2) / a.depth; });
  .separation(function(a, b) { return (a.parent == b.parent ? 1 : 2); });

var diagonal = d3.svg.diagonal()
  .projection(function(d) { return [d.y, d.x]; });

// Set up the base tree (maximum value of k):
function base(d) {
  if (d.children) {
    d._children = d.children;
    d.children = null;
  }
}

// function to set base tree for all nodes:
function baseAll(d) {
  if (d.children) {
    d.children.forEach(baseAll);
    base(d);
  }
}

// Build the tree of a given size:
function build(d, k, data) {
  if (d._children) {
	  var numchildren = 0;
	  for (var j = 0; j < d._children.length; j++) {
	    if (data[k-1][d._children[j].name] == 1) {
		    if (d.children == null) { 
		      d.children = []; 
		    };
		    //debugger;
		    d.children.push(d._children[j]);
		    d.children[numchildren].size = data[max_k + k - 1][d._children[j].name];
		    d.children[numchildren].label = data[2*max_k + k - 1][d._children[j].name] + " (" + data[max_k + k - 1][d._children[j].name] + ")";
		    numchildren++;
	    }
	  }
  }
}

// function to build all nodes of a given summary tree:
function buildAll(d, k, data) {
  build(d, k, data);
  if (d.children) {
    for (var j = 0; j < d.children.length; j++) {
      buildAll(d.children[j], k, data);
    }
  }
}

// function to read in the value of k from the form
// and draw the corresponding k-node summary tree
function changek(event){
  var new_k = Math.floor(+document.getElementById("newk").value); // ~~
  if (isNaN(new_k))
    return false;
  set_k(Math.max(min_k, Math.min(max_k, new_k)));    
  updateTree();
  return false;
}

// draw a tree given the json, data, and value of k:
function computeTree(v) {
  var st = JSON.parse(JSON.stringify(basetree));
  buildAll(st, v, basedata);
  return st;
}

// compute the nearest ancestor in the old tree, and it will transition from this ancestor's old location
function enterAncestor_xy(d) {
  var a, ancest, temp;
  if (d.name != "1") {
    while (a != "1") {
      temp = d.parent;
      a = basedata[old_k - 1][temp.name];
      d = temp;
    }
    ancest = [temp.x0, temp.y0];
  } else {
    ancest = [d.x0, d.y0];
  }
  return ancest;
}

// compute the nearest ancestor in the new tree, and the exiting node will transition to this ancestor's new location
function exitAncestor_xy(d) {
  var a, ancest, temp;
  if (d.name != "1"){
    while (a != "1") {
      temp = d.parent;
      a = basedata[current_k - 1][temp.name];
      d = temp;
    }
    ancest = [new_location[temp.name].x, new_location[temp.name].y];
  } else {
    ancest = [d.x0, d.y0];
  }
  return ancest;
}

// read in the data:
d3.json("basetree.json", function(error, json) {
  d3.csv("info.csv", function(error, data) {

    // remove the children from the base tree
    // and store them as "_children" (collapsed):
    root = json;
    baseAll(root);
    root.x0 = height/2;
    root.y0 = 0;
    root.label = "flare";
    root.size = 0;

    // set up a copy of root called basetree:
    basetree = JSON.parse(JSON.stringify(root));
    current_tree = basetree;

    // save the baseline data as a global variable:
    basedata = data;

    AddLegend();

    maxPrint = vis.append("text")
      .attr("x", 100)
      .attr("y", -5)
      .attr("transform", function(d) { return "translate(0)"; })
      .attr("text-anchor", "start")
      .attr("stroke", "black")
      .attr("stroke-width", 0.5)
      .style("fill-opacity", 1)
      .text(Math.max(divisor[current_k - 1]*100))
      .style("font", "10px sans-serif")
      .attr("text-anchor", "end");
    
    updateTree();

  });
});

function AddLegend() {
  var legend = vis.append("rect")
    .attr("x", 0)
    .attr("y", -30)
    .attr("height", 10)
    .attr("width", 100)
    .attr("fill", "#6699cc")
    .attr("stroke", "#6699cc");

  vis.append("text")
    .attr("x", 0)
    .attr("y", -35)
    .attr("transform", function(d) { return "translate(0)"; })
    .attr("text-anchor", "start")
    .attr("stroke", "black")
    .attr("stroke-width", 0.5)
    .style("fill-opacity", 1)
    .text("Size Legend (kb)")
    .style("font", "10px sans-serif");

  vis.append("text")
    .attr("x", 0)
    .attr("y", -5)
    .attr("transform", function(d) { return "translate(0)"; })
    .attr("text-anchor", "start")
    .attr("stroke", "black")
    .attr("stroke-width", 0.5)
    .style("fill-opacity", 1)
    .text("0")
    .style("font", "10px sans-serif");
}

// increment k button/link:
d3.select("#increase_k").on("click", function() {
  set_k(Math.min(current_k + 1, max_k));
  updateTree();
});

// decrement k button/link:
d3.select("#decrease_k").on("click", function() {
  set_k(Math.max(current_k - 1, min_k));
  updateTree();
});


// re-draw the tree for the new value of k:
function updateTree() {

  tree = d3.layout.tree()
    .size([10 + 12.03*current_k, width])
  //.attr("transform", "translate(" + 20 + "," + 0 + ")");
  //.separation(function(a, b) { return (a.parent == b.parent ? 1 : 2) / a.depth; });
    .separation(function(a, b) { return (a.parent == b.parent ? 1 : 2); });

  var duration = d3.event && d3.event.altKey ? 5000 : 800;

  // Stash the incoming tree as the global variable old_tree:
  old_tree = current_tree;

  // Compute the new tree layout
  current_tree = computeTree(current_k);
  var nodes = tree.nodes(current_tree);

  // Normalize for fixed-depth.
  nodes.forEach(function(d) { d.y = d.depth * fixed_depth; });

  // stash the (x,y)-locations of the new nodes in an array:
  new_location = {};
  nodes.forEach(function(d) {
    new_location[d.name] = d;
    new_location[d.name].x = d.x;
    new_location[d.name].y = d.y;
  });

  // compute the locations of the old nodes:
  var old_nodes = tree.nodes(old_tree);

  // Normalize for fixed-depth.
  old_nodes.forEach(function(d) { d.y = d.depth * fixed_depth; });

  // set an object to refer to the old nodes by name:
  var old_names = {};
  old_nodes.forEach(function(n) { old_names[n.name] = n;} );

  // manually set the old locations for all entering nodes:
  nodes.forEach( function(n) { 
    var old_node = old_names[n.name];
    if (old_node != null) {
      n.x0 = old_node.x0;
      n.y0 = old_node.y0;
    }
  });

  // compute the links for the new tree:
  var links = tree.links(nodes);

  // Update the nodes:
  var node = vis.selectAll("g.node")
    .data(nodes, function(d) { return d.name; });

  // Enter any new nodes at the previous position of their 
  // most recent ancestor in the old tree:
  var nodeEnter = node.enter().append("svg:g")
    .attr("class", "node")
    .attr("transform", function(d) {
      var ac = enterAncestor_xy(d);
      return "translate(" + ac[1] + "," + ac[0] + ")"; 
    });

  nodeEnter.append("svg:rect")
    .attr("height", 1e-6)
    .attr("width", 1e-6)
    .attr("x", 0)
    .attr("y", -5)
  //.style("fill", "lightsteelblue");

  nodeEnter.append("svg:text")
    .attr("x", function(d) {
      var adjust;
      if (d.size == 0) {
        adjust = -3;
      } else {
        adjust = 0;
      }
      return adjust;
    })
    .attr("dy", ".35em")
    .attr("transform", function(d) { return "translate(0)"; })
    .attr("text-anchor", function(d) {
      var anchor;
      if (d.size == 0) {
        anchor = "end";
      } else {
        anchor = "start";
      }
      return anchor;
    })
  //.attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
    .text(function(d) { return d.label; })
    .style("fill-opacity", 1e-6);

  // Transition nodes to their new position.
  var nodeUpdate = node.transition()
    .duration(duration)
    .attr("transform", function(d) { 
      return "translate(" + d.y + "," + d.x + ")"; 
    });

  nodeUpdate.select("rect")
    .attr("height", 10)
    .attr("width", function(d) { return d.size/divisor[current_k - 1] ; })
    .attr("x", 0)
    .attr("y", -5)
  //.style("fill", "lightsteelblue");

  nodeUpdate.select("text")
    .attr("x", function(d) {
      var adjust;
      if (d.size == 0) {
        adjust = -3;
      } else {
        adjust = 0;
      }
      return adjust;
    })
    .attr("dy", ".35em")
    .attr("transform", function(d) { return "translate(0)"; })
    .attr("text-anchor", function(d) {
      var anchor;
      if (d.size == 0) {
        anchor = "end";
      } else {
        anchor = "start";
      }
      return anchor;
    })
    .text(function(d) { return d.label; })
    .style("fill-opacity", 1);

  // Transition exiting nodes to the new position of their most recent ancestor in the new tree:
  var nodeExit = node.exit().transition()
    .duration(duration)
    .attr("transform", function(d) { 
      var ac = exitAncestor_xy(d); // this function calls the global variable new_locations
      return "translate(" + ac[1] + "," + ac[0] + ")"; 
    })
    .remove();

  nodeExit.select("rect")
    .attr("height", 1e-6)
    .attr("width", 1e-6);

  nodeExit.select("text")
    .style("fill-opacity", 1e-6);

  // Update the links
  var link = vis.selectAll("path.link")
    .data(tree.links(nodes), function(d) { return d.target.name; });

  // Enter any new links:
  link.enter().insert("svg:path", "g")
    .attr("class", "link")
    .attr("d", function(d) {
      var ac = enterAncestor_xy(d.target);
      var o = {x: ac[0], y: ac[1]};
      return diagonal({source: o, target: o});
    })
    .transition()
    .duration(duration)
    .attr("d", diagonal);

  // Transition links to their new position.
  link.transition()
    .duration(duration)
    .attr("d", diagonal);

  // Transition exiting nodes
  link.exit().transition()
    .duration(duration)
    .attr("d", function(d) {
      var ac = exitAncestor_xy(d.target);
      var o = {x: ac[0], y: ac[1]};
      return diagonal({source: o, target: o});
    })
    .remove();

  // Stash the old positions for transition.
  nodes.forEach(function(d) {
    d.x0 = d.x;
    d.y0 = d.y;
  });

  // set the old_k to the current_k:
  old_k = current_k;
  
  maxPrint.remove();
  
  maxPrint = vis.append("text")
    .attr("x", 100)
    .attr("y", -5)
    .attr("transform", function(d) { return "translate(0)"; })
    .attr("text-anchor", "start")
    .attr("stroke", "black")
    .attr("stroke-width", 0.5)
    .style("fill-opacity", 1)
    .text(Math.floor(divisor[current_k - 1]*100))
    .style("font", "10px sans-serif")
    .attr("text-anchor", "end");
}

