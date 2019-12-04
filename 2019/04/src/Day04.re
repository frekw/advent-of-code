let digits = n => {
  let rec do_ = (acc, n) =>
    switch (n) {
    | n when n <= 0 => acc
    | n =>
      let x = n mod 10;
      do_([x, ...acc], n / 10);
    };

  do_([], n);
};

let count = (xs, y) => Belt.List.keep(xs, x => x == y)->Belt.List.length;
let hasAdjecent = xs => Belt.List.(some(xs, x => count(xs, x) == 2));

let isIncreasing = xs => {
  let rec run = (last, xs) =>
    switch (last, xs) {
    | (n, [m, ...rest]) when n == m => run(n, rest)
    | (n, [m, ...rest]) when m > n => run(m, rest)
    | (_, []) => true
    | _ => false
    };

  run(min_int, xs);
};

let range = (from, to_) => {
  let rec run = (acc, from, to_) =>
    switch (to_ - 1) {
    | n when n == from => [n, ...acc]
    | n => run([n, ...acc], from, n)
    };

  run([], from, to_ + 1);
};

let run = ((from, to_)) =>
  Belt.List.(
    range(from, to_)
    ->map(digits)
    ->keep(xs => length(xs) == 6)
    ->keep(hasAdjecent)
    ->keep(isIncreasing)
    ->length
  );

Js.Console.log(run(Input.input));
// ->Belt.List.forEach(Js.Console.log);