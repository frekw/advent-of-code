// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Input$MyNewProject = require("./Input.bs.js");

function divf(a, b) {
  return a / b;
}

function minusf(a, b) {
  return a - b;
}

function add(a, b) {
  return a + b | 0;
}

function fuelForMass(m) {
  return Math.floor(m / 3) - 2 | 0;
}

function totalFuelForMass(x) {
  var n = fuelForMass(x);
  if (n < 1) {
    return 0;
  } else {
    return n + totalFuelForMass(n) | 0;
  }
}

function fuelFor(input, fuelFunction) {
  return Belt_Array.reduce(Belt_Array.map(input, fuelFunction), 0, add);
}

console.log(fuelFor(Input$MyNewProject.input, totalFuelForMass));

exports.divf = divf;
exports.minusf = minusf;
exports.add = add;
exports.fuelForMass = fuelForMass;
exports.totalFuelForMass = totalFuelForMass;
exports.fuelFor = fuelFor;
/*  Not a pure module */
