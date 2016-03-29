
let html = {|
<html>
<head>
<title>Prof. Alloc</title>
<style>
body {
  font: 10px sans-serif;
}

.axis path,
.axis line {
  fill: none;
  stroke: #000000;
  shape-rendering: crispEdges;
}

.layer text {
  text-anchor: end;
}
</style>
<script type="text/javascript" src="http://d3js.org/d3.v3.min.js" charset="utf-8">
</script>
</head>
<body>
<script type="text/javascript" src="graph.js"></script>
</body>
</html>
|}

let js = {|
var margin = {top: 100, right: 200, bottom: 100, left: 200};

var width = 1280 - margin.left - margin.right;

var height = 640 - margin.top - margin.bottom;

var popup_padding = 4;

var x = d3.scale.linear().range([0, width]);

var y = d3.scale.linear().range([height, 0]);

var color1 = d3.scale.category20();
var color2 = d3.scale.category20c();

var xAxis = d3.svg.axis().scale(x).orient("bottom");

var yAxis = d3.svg.axis().scale(y).orient("left");

var area = d3.svg.area()
    .x((d) => { return x(d.time); })
    .y0(function(d) { return y(d.y0); })
    .y1(function(d) { return y(d.y0 + d.y); });

var stack = d3.layout.stack()
    .values(function(d) { return d.values; });

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var body =
  svg.append("g")
    .on("mouseover", function() {
      popup.transition().duration(500).style("opacity", 1);
    })
    .on("mouseout", function() {
      popup.transition().duration(10).style("opacity", 0);
    })
    .on("mousemove", function(d) {
      var x = d3.event.pageX - margin.left;
      var y = d3.event.pageY - margin.top;
      popup.attr("transform", "translate(" + x + "," + y + ")");
    });

var popup =
  svg.append("g")
    .attr("id", "popup")
    .attr("pointer-events", "none")
    .style("opacity", 0);

popup.append("rect")
  .attr("fill", "#000000")
  .attr("stroke", "#969696")
  .attr("rx", "5")
  .attr("ry", "5")
  .style("opacity", .6);

popup.append("text")
  .attr("fill", "#FFFFFF");

svg.append("g")
  .attr("class", "axis")
  .attr("id","xaxis")
  .attr("transform", "translate(0," + height + ")");

svg.append("g")
  .attr("class", "axis")
  .attr("id","yaxis");

function graph(input, dispatch) {

  var locations = d3.entries(input.locations);
  var snapshots = input.snapshots;

  color1.domain(locations.map(function (location)
     { return location.key }));
  color2.domain(locations.map(function (location)
     { return location.key }).reverse());

  var layers = stack(locations.map(function(location) {
    return {
      addr: location.key,
      display: location.value.display,
      foreign: location.value.foreign,
      depth: location.value.depth,
      values: snapshots.map(function(d) {
        return {time: d.time, y: d.values[location.key]};
      })
    };
  }));

  x.domain(d3.extent(snapshots, function(d) { return d.time; }));
  y.domain([0, d3.max(snapshots, function(d) {
    return d3.sum(layers, function (b) { return d.values[b.addr]; });
  })]);

  svg.select("#xaxis").call(xAxis);
  svg.select("#yaxis").call(yAxis);

  var layer =
    body.selectAll(".layer")
      .data(layers);

  layer.enter()
    .append("g")
    .attr("class", "layer")
    .append("path", "#popup")
    .attr("class", "area");

  layer.select("path")
    .attr("d", function(d) { return area(d.values); })
    .on("mouseenter", function(d) {
      d3.select(this).attr("stroke", "#000000");
      var x = d3.event.pageX - margin.left;
      var y = d3.event.pageY - margin.top;
      popup.attr("transform", "translate(" + x + "," + y + ")");
      var text = popup.select("text");
      text.text(d.display);
      if(d.foreign) {
        text.attr("fill", "#66FF00");
      } else {
        text.attr("fill", "#FFFFFF");
      }
      var box = text.node().getBBox();
      popup.select("rect")
        .attr("x", box.x - popup_padding)
        .attr("y", box.y - popup_padding)
        .attr("width", box.width + popup_padding*2)
        .attr("height", box.height + popup_padding*2);
    })
    .on("mouseleave", function(d) {
      d3.select(this).attr("stroke", "none");
    })
    .on("click", function (d) { dispatch.select(d.addr) })
    .attr("fill", function(d) {
       if(d.depth % 2 == 0) {
         return color1(d.addr);
       } else {
         return color2(d.addr);
       }
     });

  layer.exit().remove();

}

function fetch(addr, dispatch) {
  var xmlhttp = new XMLHttpRequest();
  xmlhttp.onload = function () {
    if(xmlhttp.status == 200) {
      var state = JSON.parse(xmlhttp.responseText);
      graph(state, dispatch);
    }
  };
  xmlhttp.open("GET", addr + "/series.json", true);
  xmlhttp.send();
}

var path = "data";
var dispatch = d3.dispatch("select");
fetch(path, dispatch);
dispatch.on("select", function (addr) {
  path = path + "/" + addr;
  fetch(path, dispatch);
});
|}
