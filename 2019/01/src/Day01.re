open Belt;

let divf = (a, b) => a /. b;
let minusf = (a, b) => a -. b;
let add = (a, b) => a + b;
let fuelForMass = m => m
->float_of_int
->divf(_, 3.)
->floor
->minusf(_, 2.)
->int_of_float;

let rec totalFuelForMass = x => switch (fuelForMass(x)) {
  | n when n < 1 => 0;
  | n => n + totalFuelForMass(n);
};

let fuelFor = (input, fuelFunction) =>
  input
  ->Array.map(fuelFunction)
  ->Array.reduce(0, add);

// Js.Console.log(fuelFor(Input.input, fuelForMass));
Js.Console.log(fuelFor(Input.input, totalFuelForMass));