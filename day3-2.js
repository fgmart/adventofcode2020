// Advent of Code Day 3 Part 2
// Fred Martin, fredm@alum.mit.edu
// Dec 5, 2020

// not very elegant, I kinda brute-forced it
// added a half-col and checked every other row for the right 1, down 2 case
// I suppose I could have made arrays or something for the variables
//   and increments

const lineByLine = require('n-readlines');
const fs = require('fs')
const fn = "day3-input.txt"

const liner = new lineByLine(fn);

var row= 0;
var r1D1Col= 0; r1D1Tree= 0;
var r3D1Col= 0; r3D1Tree= 0;
var r5D1Col= 0; r5D1Tree= 0;
var r7D1Col= 0; r7D1Tree= 0;
var r1D2Col= 0; r1D2Tree= 0;

var trees=0;

var width;

let line;
 
while (line = liner.next()) {
    let mapstr = line.toString();
    let mapchar;

    // skip the first row
    if (row == 0) {
	row++;
	continue;
    }
    
    width = mapstr.length;

    // right 1 down 1
    r1D1Col+= 1;
    mapchar = mapstr.charAt(r1D1Col % width);
    if (mapchar == '#') 
	r1D1Tree++;

    // right 3 down 1
    r3D1Col+= 3;
    mapchar = mapstr.charAt(r3D1Col % width);
    if (mapchar == '#') 
	r3D1Tree++;

    // right 5 down 1
    r5D1Col+= 5;
    mapchar = mapstr.charAt(r5D1Col % width);
    if (mapchar == '#') 
	r5D1Tree++;

    // right 7 down 1
    r7D1Col+= 7;
    mapchar = mapstr.charAt(r7D1Col % width);
    if (mapchar == '#') 
	r7D1Tree++;

    // right 1 down 2
    r1D2Col+= 0.5;
    if ((row % 2) == 0) {
	mapchar = mapstr.charAt(r1D2Col % width);
	if (mapchar == '#') 
	    r1D2Tree++;
    }
    row++;
    
}

console.log("Tree product is " + r1D1Tree * r3D1Tree * r5D1Tree * r7D1Tree *
	    r1D2Tree)
