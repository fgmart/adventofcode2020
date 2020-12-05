// Advent of Code Day 3 Part 1
// Fred Martin, fredm@alum.mit.edu
// Dec 5, 2020

// const readline = require('readline')
const lineByLine = require('n-readlines');
const fs = require('fs')
const fn = "day3-input.txt"

const liner = new lineByLine(fn);

var row=0; col=0;

var trees=0;

var width;

let line;
 
while (line = liner.next()) {
    let mapstr = line.toString();

    if (row == 0) {
	row++;
	continue;
    }
    
    width = mapstr.length;

    col = col + 3;

    console.log(mapstr + " row " + row + " col " + col);

    let mapobj = mapstr.charAt(col % width);
    if (mapobj == '#') {
	trees++;
	console.log("Tree!");
    } else {
	console.log("      no tree");
    }
    row++;
    
    //    console.log(line.toString());
}

console.log("Found " + trees + " trees in " + row + " rows.");




