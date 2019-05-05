////////////////////////////////////////////////////////////
//////////////////////// Set-up ////////////////////////////
////////////////////////////////////////////////////////////
var screenWidth = $(window).width(),
    mobileScreen = (screenWidth > 400 ? false : true);

var margin = {left: 50, top: 10, right: 50, bottom: 10},
    width = Math.min(screenWidth, 800) - margin.left - margin.right,
    height = (mobileScreen ? 300 : Math.min(screenWidth, 800)*5/6) - margin.top - margin.bottom;

var svg = d3.select("#chart").append("svg")
    .attr("width", (width + margin.left + margin.right))
    .attr("height", (height + margin.top + margin.bottom));

var wrapper = svg.append("g").attr("class", "chordWrapper")
    .attr("transform", "translate(" + (width / 2 + margin.left) + "," + (height / 2 + margin.top) + ")");;

var outerRadius = Math.min(width, height) / 2  - (mobileScreen ? 80 : 100),
    innerRadius = outerRadius * 0.95,
    pullOutSize = (mobileScreen? 20 : 50),
    opacityDefault = 0.7, //default opacity of chords
    opacityLow = 0.02; //hover opacity of those chords not hovered over

////////////////////////////////////////////////////////////
////////////////////////// Data ////////////////////////////
////////////////////////////////////////////////////////////

var Names = ["Northeast","Central","South","West","","West","South","Central","Northeast",""];
//var Names = ["West","South","Central","Northeast","","Northeast","Central","South","West",""];

var respondents = 834098, //Total number of physicians (i.e. the number that makes up the total group)
    emptyPerc = 0.25, //What % of the circle should become empty
    emptyStroke = Math.round(respondents*emptyPerc);
var matrix = [
    [0,0,0,0,0,32232,49570,18307,176625,0], //NE
    [0,0,0,0,0,25959,34589,121016,13263,0], //C
    [0,0,0,0,0,28987,155223,18827,15378,0], //S
    [0,0,0,0,0,102520,22654,9742,9206,0], //W
    [0,0,0,0,0,0,0,0,0,emptyStroke], //Dummy stroke
    [9206,9742,22654,102520,0,0,0,0,0,0], //W
    [15378,18827,155223,28987,0,0,0,0,0,0], //S
    [13263,121016,34589,25959,0,0,0,0,0,0], //C
    [176625,18307,49570,32232,0,0,0,0,0,0], //NE
    [0,0,0,0,emptyStroke,0,0,0,0,0] //Dummy stroke
];
//Calculate how far the Chord Diagram needs to be rotated clockwise to make the dummy
//invisible chord center vertically
var offset = (2 * Math.PI) * (emptyStroke/(respondents + emptyStroke))/4;

//Custom sort function of the chords to keep them in the original order
function customSort(a,b) {
    return 1;
};

//Custom sort function of the chords to keep them in the original order
var chord = customChordLayout() //d3.layout.chord()//Custom sort function of the chords to keep them in the original order
    .padding(.02)
    .sortChords(d3.descending) //which chord should be shown on top when chords cross. Now the biggest chord is at the bottom
    .matrix(matrix);

var arc = d3.svg.arc()
    .innerRadius(innerRadius)
    .outerRadius(outerRadius)
    .startAngle(startAngle) //startAngle and endAngle now include the offset in degrees
    .endAngle(endAngle);

var path = stretchedChord()
    .radius(innerRadius)
    .startAngle(startAngle)
    .endAngle(endAngle)
    .pullOutSize(pullOutSize);

////////////////////////////////////////////////////////////
//////////////////// Draw outer Arcs ///////////////////////
////////////////////////////////////////////////////////////

var g = wrapper.selectAll("g.group")
    .data(chord.groups)
    .enter().append("g")
    .attr("class", "group")
    .on("mouseover", fade(opacityLow))
    .on("mouseout", fade(opacityDefault));

g.append("path")
    .style("stroke", function(d,i) { return (Names[i] === "" ? "none" : "#00A1DE"); })
    .style("fill", function(d,i) { return (Names[i] === "" ? "none" : "#00A1DE"); })
    .style("pointer-events", function(d,i) { return (Names[i] === "" ? "none" : "auto"); })
    .attr("d", arc)
    .attr("transform", function(d, i) { //Pull the two slices apart
	d.pullOutSize = pullOutSize * ( d.startAngle + 0.001 > Math.PI ? -1 : 1);
	return "translate(" + d.pullOutSize + ',' + 0 + ")";
    });


////////////////////////////////////////////////////////////
////////////////////// Append Names ////////////////////////
////////////////////////////////////////////////////////////

//The text also needs to be displaced in the horizontal directions
//And also rotated with the offset in the clockwise direction
g.append("text")
    .each(function(d) { d.angle = ((d.startAngle + d.endAngle) / 2) + offset;})
    .attr("dy", ".35em")
    .attr("class", "titles")
    .attr("text-anchor", function(d) { return d.angle > Math.PI ? "end" : null; })
    .attr("transform", function(d,i) { 
	var c = arc.centroid(d);
	return "translate(" + (c[0] + d.pullOutSize) + "," + c[1] + ")"
	    + "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
	    + "translate(" + 55 + ",0)"
	    + (d.angle > Math.PI ? "rotate(180)" : "")
    })
    .text(function(d,i) { return Names[i]; });

////////////////////////////////////////////////////////////
//////////////////// Draw inner chords /////////////////////
////////////////////////////////////////////////////////////

var chords = wrapper.selectAll("path.chord")
    .data(chord.chords)
    .enter().append("path")
    .attr("class", "chord")
    .style("stroke", "none")
    .style("fill", "#C4C4C4")
    .style("opacity", function(d) { return (Names[d.source.index] === "" ? 0 : opacityDefault); }) //Make the dummy strokes have a zero opacity (invisible)
    .style("pointer-events", function(d,i) { return (Names[d.source.index] === "" ? "none" : "auto"); }) //Remove pointer events from dummy strokes
    .attr("d", path);	

////////////////////////////////////////////////////////////
///////////////////////// Tooltip //////////////////////////
////////////////////////////////////////////////////////////

//Arcs
g.append("title")	
    .text(function(d, i) {return Math.round(d.value) + " physicians in the " + Names[i];});

//Chords
chords.append("title")
    .text(function(d) {
	return [Math.round(d.source.value), " physicians from ", Names[d.target.index], " to ", Names[d.source.index]].join(""); 
    });

////////////////////////////////////////////////////////////
////////////////// Extra Functions /////////////////////////
////////////////////////////////////////////////////////////

//Include the offset in de start and end angle to rotate the Chord diagram clockwise
function startAngle(d) { return d.startAngle + offset; }
function endAngle(d) { return d.endAngle + offset; }

// Returns an event handler for fading a given chord group
function fade(opacity) {
    return function(d, i) {
	svg.selectAll("path.chord")
	    .filter(function(d) { return d.source.index !== i && d.target.index !== i && Names[d.source.index] !== ""; })
	    .transition("fadeOnArc")
	    .style("opacity", opacity);
    };
}//fade
