//234567890123456789012345678901234567890123456789012345678901234567890123456789
// set up some global variables
var margin = {top: 50, right: 0, bottom: 0, left: 25};
var svg_width = 3000;
var svg_height = 5000;
var basetree;  // hold a copy of the baseline tree
var basedata;  // hold a copy of the node data for k = 1, ..., max_k
var root;
var old_tree; 
var current_tree;
var new_location;
var legend_width; // width of legend bar
var node_width; // width of each level (node + space)
var node_height; // node height
var sep_sibling = 1.7;
var sep_nonsibling = 2;
var current_k;
var old_k = 1; 
var min_k = 1; 
var max_k;
var legend_color;
var node_color;  // either the same as legend or computed in R to map to
// a node's ancestor at a user-supplied level of the tree
var maxPrint;
var divisor;
var units;
var print_weights; // print the weight of each node in parentheses (T/F)
var link_height; // maybe use this later to locate entropy profile plot
// explicitly outside of the area used to draw links between nodes.
// only troublesome link is the one from root to uppermost child
var entropy;
var maxent;
var epcircle;
var lineGraph;
var ent_x;
var ent_y;

var diagonal = d3.svg.diagonal()
  .projection(function(d) { return [d.y, d.x]; });

// Set up the base tree for a given node:
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
	  var nchild = 0;
	  for (var j = 0; j < d._children.length; j++) {
      // if (this node exists 0/1 in the k-node summary tree)
	    if (data[k - 1][d._children[j].name] == 1) {
		    if (d.children == null) { // initialize children if none exist
		      d.children = []; 
		    };
		    d.children.push(d._children[j]);
        var w = max_k + k - 1;  // index of weight in basedata
        var l = 2*max_k + k - 1;  // index of label in basedata
		    d.children[nchild].size = data[w][d._children[j].name];
		    if (print_weights) {
          d.children[nchild].label = data[l][d._children[j].name] + 
            " (" + data[w][d._children[j].name] + ")";
        } else {
          d.children[nchild].label = data[l][d._children[j].name];      
        }
		    nchild++;
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

// draw a tree given the json, data, and value of k:
function computeTree(v) {
  var st = JSON.parse(JSON.stringify(basetree));
  buildAll(st, v, basedata);
  return st;
}

// compute the nearest ancestor in the old tree, and it will transition 
// from this ancestor's old location
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

// compute the nearest ancestor in the new tree, and the exiting node will 
// transition to this ancestor's new location
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

// set up div for all inputs (text, buttons, slider) to change k:
var topdiv = document.createElement("div");
topdiv.setAttribute("class", "topdiv");
document.getElementById("summarytree-drawing").appendChild(topdiv);

var kdiv = document.createElement("div");
kdiv.innerHTML = "Enter # of nodes in summary tree: k = ";
kdiv.id = "text-plus-buttons";
topdiv.appendChild(kdiv);

var ktextbox = document.createElement("input");
ktextbox.setAttribute("type", "text");
ktextbox.setAttribute("id", "newk");
ktextbox.setAttribute("size", "4");
kdiv.appendChild(ktextbox);

var ktextsubmit = document.createElement("button");
ktextsubmit.setAttribute("class", "kbutton");
ktextsubmit.setAttribute("id", "ksubmit");
ktextsubmit.innerHTML = "Submit";
kdiv.appendChild(ktextsubmit);

var decreaseButton = document.createElement("button");
decreaseButton.setAttribute("class", "kbutton");
decreaseButton.setAttribute("id", "decrease_k");
decreaseButton.innerHTML = "k--";
kdiv.appendChild(decreaseButton);

var increaseButton = document.createElement("button");
increaseButton.setAttribute("class", "kbutton");
increaseButton.setAttribute("id", "increase_k");
increaseButton.innerHTML = "k++";
kdiv.appendChild(increaseButton);

var sliderWrapper = document.createElement("div");
sliderWrapper.setAttribute("id", "lambdaInput");
sliderWrapper.setAttribute("class", "slid");
kdiv.appendChild(sliderWrapper);

var sliderDiv = document.createElement("div");
sliderDiv.setAttribute("id", "sliderdiv");
sliderDiv.setAttribute("class", "sd");
sliderWrapper.appendChild(sliderDiv);

var sliderInput = document.createElement("input");
sliderInput.setAttribute("class", "sliderinput");
sliderInput.type = "range";
sliderInput.id = "sliderButton";
sliderDiv.appendChild(sliderInput);

// Set up the plotting region for the legend + tree:
var vis = d3.select("#summarytree-drawing").append("svg:svg")
  .attr("width", svg_width + margin.left + margin.right)
  .attr("height", svg_height + margin.top + margin.bottom)
  .append("svg:g")
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// the dark gray rectangle defining the data plotting area:
vis.append("rect")
  .attr("x", 0)
  .attr("y", 0)
  .attr("width", svg_width)
  .attr("height", svg_height)
  .attr("fill", "black")
  .attr("fill-opacity", 0.00)
  .attr("stroke", "black")
  .attr("stroke-opacity", 0.00);

// the light gray rectangle defining the total plotting area
// This includes room around the data plotting area for margin labels, etc.
vis.append("rect")
  .attr("x", -margin.left)
  .attr("y", -margin.top)
  .attr("width", svg_width + margin.left + margin.right)
  .attr("height", svg_height + margin.top + margin.bottom)
  .attr("fill", "black")
  .attr("fill-opacity", 0.00);

// Set up a box to contain the entropy profile plot:
var ep = vis.append("rect")
  .attr("x", 0)
  .attr("y", 10)
  .attr("fill", "black")
  .attr("fill-opacity", 0.10)
  .attr("id", "entropy_profile");

ent_y = vis.append("text")
  .attr("class", "legend_text")
  .attr("text-anchor", "middle")
  .attr("stroke-width", 0.5)
  .text("entropy"); 

ent_x = vis.append("text")
  .attr("class", "legend_text")
  .attr("text-anchor", "middle")
  .attr("stroke-width", 0.5)
  .text("# nodes (k)"); 


// To display the value of k in the box on the webpage:
function set_k(new_k) {
  current_k = new_k;
  document.getElementById("newk").value = current_k;
}

// read in the data:
d3.json("data.json", function(error, json) {

  // Read in a few plot options:
  legend_width = json['legend.width'];
  node_width = json['node.width'];
  node_height = json['node.height'];
  units = json['units'];
  print_weights = json['print.weights'];
  legend_color = json['legend.color'];
  node_color = json['node.color'];
  entropy = json['entropy'];

  // First read the vector of divisors for each value of k:
  divisor = JSON.parse(json['divisor']);

  // set K (called 'max_k' here) based on divisor.length:
  max_k = divisor.length;

  // k = 20 is the default initial value (restricted to be between 1 and K):
  set_k(Math.max(min_k, Math.min(max_k, 20)));

  // gather the data on each of the K trees from the input array 'data':
  data = json['out'];
  //debugger;

  // set width and height of entropy profile plot to match legend_width:
  ep.attr("width", legend_width);
  ep.attr("height", legend_width);
  ent_y.attr("x", 0);
  ent_y.attr("y", legend_width/2);
  ent_y.attr("transform", function(d) { 
    return "translate(-" + (3 + legend_width/2) + ", " + legend_width/2 + 
           ")rotate(-90)";
  });
  ent_x.attr("x", legend_width/2);
  ent_x.attr("y", legend_width + 20);


  // data-dependent values associated with the slider input:
  sliderInput.min = 1;
  sliderInput.max = max_k;
  sliderInput.step = 1;
  sliderInput.value = current_k;

  // Create the svg to contain the slider scale:
  var scaleContainer = d3.select("#sliderdiv").append("svg")
	  .attr("width", 310)
	  .attr("height", 25);

  var sliderScale = d3.scale.linear()
	  .domain([1, max_k])
	//.range([0, 300])
	  .range([7.5, 292.5]);
  // trimmed by 7.5px on each side to align with slider button on ends
	  //.nice(); this led to problems so I replaced it with .tickValues()

  // Set up a manual array of tick labels because .nice() was creating 
  // labels outside of the domain (max of 160 when max_k was 150 for example)
  tickvector = [1];
  for (i = 1; i <= 6; i++) {
    tickvector.push(Math.round(i*max_k/6));
  }

  // adapted from http://bl.ocks.org/mbostock/1166403
  var sliderAxis = d3.svg.axis()
	  .scale(sliderScale)
	  .orient("bottom")
	  .tickSize(10)
	  .tickSubdivide(true)
    .tickValues(tickvector);

  // group to contain the elements of the slider axis:
  var sliderAxisGroup = scaleContainer.append("g")
	  .attr("class", "slideraxis")
	  .attr("margin-top", "0px")
	  .call(sliderAxis);

  // manually change the first slider axis tick label to "1":
  // warning: this might only work because the first slider axis tick label 
  // is the first text element in the vis (for now)
  //d3.select("text")[0][0].innerHTML = "1";
  // Note: no longer necessary as I am manually setting tick labels (above)

  // When the value of lambda changes, update the visualization
  d3.select("#sliderButton")
    .on("mouseup", function() {
      var new_k = +this.value;
      document.getElementById("newk").value = new_k;
      if (isNaN(new_k))
        return false;
      set_k(Math.max(min_k, Math.min(max_k, new_k)));
      updateTree();
      return false;
    });

  // function to read in the value of k from the form
  // and draw the corresponding k-node summary tree
  d3.select("#ksubmit").on("click", function() {
    var new_k = Math.floor(+document.getElementById("newk").value);
    if (isNaN(new_k))
      return false;
    set_k(Math.max(min_k, Math.min(max_k, new_k)));    
    // update slider to reflect new value of k:
    document.getElementById("sliderButton").value = current_k;
    updateTree();
    return false;
  });

  // increment k button/link:
  d3.select("#increase_k").on("click", function() {
    set_k(Math.min(current_k + 1, max_k));
    // update slider to reflect new value of k:
    document.getElementById("sliderButton").value = current_k;
    updateTree();
  });

  // decrement k button/link:
  d3.select("#decrease_k").on("click", function() {
    set_k(Math.max(current_k - 1, min_k));
    // update slider to reflect new value of k:
    document.getElementById("sliderButton").value = current_k;
    updateTree();
  });


  // remove the children from the base tree
  // and store them as "_children" (collapsed):
  root = JSON.parse(json['basetree']);
  baseAll(root);
  root.x0 = 0;
  root.y0 = 0;
  root.label = data[2*data.length/3][1];
  root.size = 0;
  //debugger;

  // set up a copy of root called basetree:
  basetree = JSON.parse(JSON.stringify(root));
  current_tree = basetree;

  // save the baseline data as a global variable:
  basedata = data;

  AddLegend();

  // scale entropy values:
  var entropyx = d3.scale.linear()
    .domain([1, max_k])
    .range([1, legend_width]);

  // From: http://stackoverflow.com/questions/4020796/
  // finding-the-max-value-of-an-attribute-in-an-array-of-objects
  maxent = Math.max.apply(Math, entropy.map(function(o) { return o.y;}));
  var entropyy = d3.scale.linear()
    .domain([0, maxent])
    .range([legend_width, 0]);

  // function to plot a line:
  var lineFunction = d3.svg.line()
    .x(function(d) { return entropyx(d.x); })
    .y(function(d) { return entropyy(d.y); })
    .interpolate("linear");

  //The line SVG Path we draw
  lineGraph = vis.append("path")
    .attr("d", lineFunction(entropy))
    .attr("class", "ep")
    .attr("transform", function(d) { return "translate(0, 10)";});

  // append a circle (with no location yet):
  epcircle = vis.append("circle")
    .attr("fill", "black")
    .attr("id", "entropycircle")
    .attr("r", 5);

  updateTree();

});



function AddLegend() {
  var legend = vis.append("rect")
    .attr("x", 0)
    .attr("y", -30)
    .attr("height", 10)
    .attr("width", legend_width)
    .attr("fill", legend_color)
    .attr("stroke", legend_color);

  vis.append("text")
    .attr("class", "legend_text")
    .attr("x", 0)
    .attr("y", -35)
    .attr("transform", function(d) { return "translate(0)"; })
    .attr("text-anchor", "start")
    .attr("stroke-width", 0.5)
    .style("fill-opacity", 1)
    .text("Node Weight" + (units == "" ? "" : " (" + units + ")"));

  vis.append("text")
    .attr("class", "legend_text")
    .attr("x", 0)
    .attr("y", -5)
    .attr("transform", function(d) { return "translate(0)"; })
    .attr("text-anchor", "start")
    .attr("stroke-width", 0.5)
    .style("fill-opacity", 1)
    .text("0");
  
  maxPrint = vis.append("text")
    .attr("class", "legend_text")
    .attr("x", legend_width)
    .attr("y", -5)
    .attr("transform", function(d) { return "translate(0)"; })
    .attr("text-anchor", "start")
    .attr("stroke-width", 0.5)
    .style("fill-opacity", 1)
    .text(Math.max(divisor[current_k - 1]*legend_width))
    .attr("text-anchor", "end");
}


// re-draw the tree for the new value of k:
function updateTree() {

  tree = d3.layout.tree()
    .separation(function(a, b) { 
      return (a.parent == b.parent ? sep_sibling : sep_nonsibling); 
    })
    .nodeSize([node_height, node_width]); // [height, width]
  // setting .nodeSize() means that tree.size() will be ignored.
  // using tree.size() sets the size of the whole tree, and the 
  // content will automatically be squeezed inside this region.
  // tree.size() keeps the vis compact but for K > about 100, the nodes
  // typically start to overlap if the region is a single screen.
  // better to use nodeSize() to ensure good spacing between nodes, 
  // even if the vis extends beyond 1 screen in width or height, requiring
  // the user to scroll to the right or down.

  // duration of transitions:
  var duration = d3.event && d3.event.altKey ? 5000 : 600;

  // Stash the incoming tree as the global variable old_tree:
  old_tree = current_tree;

  // Compute the new tree layout
  current_tree = computeTree(current_k);

  // add in the information for the root:
  current_tree.size = basedata[max_k + current_k - 1]["1"];
  if (print_weights) {
    current_tree.label = basedata[2*max_k + current_k - 1]["1"] + " (" + 
      basedata[max_k + current_k - 1]["1"] + ")";
  } else {
    current_tree.label = basedata[2*max_k + current_k - 1]["1"]
  }
  //debugger;

  // create the nodes and links objects:
  var nodes = tree.nodes(current_tree);
  var links = tree.links(nodes);

  // Shift everything down:
  var shift = 0;
  nodes.forEach(function(d) { 
    if (d.x < shift) {
      shift = d.x;
    }
  });
  if (shift > -260) { shift = -260};
  
  nodes.forEach(function(d) {
    d.x = d.x - shift + node_height/2;
  });

  // stash the (x,y)-locations of the new nodes in an array:
  new_location = {};
  nodes.forEach(function(d) {
    new_location[d.name] = d;
    new_location[d.name].x = d.x;
    new_location[d.name].y = d.y;
  });

  // compute the locations of the old nodes:
  var old_nodes = tree.nodes(old_tree);

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
    .attr("y", -node_height/2)
    .style("stroke", function(d) {
      return (d.depth == 0 ? "#ffffff" : node_color[d.name]);
    })
    .style("fill", function(d) {
      return (d.depth == 0 ? "#ffffff" : node_color[d.name]);
    })

  nodeEnter.append("svg:text")
    .attr("x", function(d) {
      var adjust;
      if (d.size == 0 & d.depth != 0) {
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
      if (d.size == 0 & d.depth != 0) {
        anchor = "end";
      } else {
        anchor = "start";
      }
      return anchor;
    })
    .text(function(d) { return d.label; })
    .style("fill-opacity", 1e-6);

  // get width of the text svg element that holds the root's label:
  // this relies on the root always being the 11th (index = 10)
  // text element on the page!
  root_text_width = d3.selectAll("text")[0][12].getBBox().width;

  // Transition nodes to their new position.
  var nodeUpdate = node.transition()
    .duration(duration)
    .attr("transform", function(d) { 
      return "translate(" + d.y + "," + d.x + ")"; 
    });

  nodeUpdate.select("rect")
    .attr("height", node_height)
    .attr("width", function(d) { 
      return ((d.depth != 0 || current_k == 1) ? 
              d.size/divisor[current_k - 1] : root_text_width); 
    })
    .attr("x", 0)
    .attr("y", -node_height/2)
    .style("fill", function(d) {
      return ((d.depth != 0 || current_k == 1) ? 
              node_color[d.name] : "#ffffff")
    });

  nodeUpdate.select("text")
    .attr("x", function(d) {
      var adjust;
      if (d.size == 0 & d.depth != 0) {
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
      if (d.size == 0 & d.depth != 0) {
        anchor = "end";
      } else {
        anchor = "start";
      }
      return anchor;
    })
    .text(function(d) { return d.label; })
    .style("fill-opacity", 1);

  // Transition exiting nodes to the new position of their most recent 
  //ancestor in the new tree:
  var nodeExit = node.exit().transition()
    .duration(duration)
    .attr("transform", function(d) { 
      // this function calls the global variable new_locations
      var ac = exitAncestor_xy(d);
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

  // Transition exiting links:
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

  // print the value of the bar width in the legend:
  maxPrint.text(Math.round(Math.max(divisor[current_k - 1]*legend_width)));

  // get height of link from root to highest child
  // (might be used later to offset entropy profile plot)
  link_height = d3.select("path.link")[0][0].getBBox().height;

  // scale entropy values:
  var entropyx = d3.scale.linear()
    .domain([1, max_k])
    .range([1, legend_width]);

  // From: http://stackoverflow.com/questions/4020796/
  // finding-the-max-value-of-an-attribute-in-an-array-of-objects
  var entropyy = d3.scale.linear()
    .domain([0, maxent])
    .range([legend_width, 0]);

  // http://stackoverflow.com/questions/25655372/d3-steady-horizontal-
  // transition-along-an-svg-path
  var lookup = [];
  var granularity = 1000;
  var l = lineGraph.node().getTotalLength();
  for(var i = 0; i <= granularity; i++) {
    var p = lineGraph.node().getPointAtLength(l * (i/granularity));
    lookup.push({
      x: p.x,
      y: p.y + 10
    })
  }
    
  // https://gist.github.com/mbostock/1313857
  // Returns an attrTween for translating along the specified path element.
  var xBisect = d3.bisector(function(d) { return d.x; }).left;

  function translateAlong(path, nw, old) {
    var l = path.getTotalLength();
    return function(d, i, a) {
      return function(t) {
        var scalar = (nw - old)/(max_k - 1);
        var start = (old - 1)/(max_k - 1);
        var index = xBisect(lookup, (t * scalar + start)*legend_width);
        var p = lookup[index];
        return "translate(" + p.x + "," + p.y + ")";
      };
    };
  }

  // update the location of the point on the entropy profile plot:
  epcircle.transition()
    .duration(600)
    .attrTween("transform", translateAlong(lineGraph.node(), current_k, old_k));
  
  // set the old_k to the current_k:
  old_k = current_k;
  
}

