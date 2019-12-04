module T =
  Belt.Id.MakeComparable({
    type t = (int, int);
    let cmp = ((a0, a1), (b0, b1)) =>
      switch (Pervasives.compare(a0, b0)) {
      | 0 => Pervasives.compare(a1, b1)
      | c => c
      };
  });

/* list(option('a)) => option(list('a)) */
let combine = xs =>
  Belt.List.reduceReverse(xs, Some([]), (acc, x) =>
    Belt.Option.flatMap(acc, xs => Belt.Option.map(x, x => [x, ...xs]))
  );
let split = Rationale.Function.flip(Js.String.split);
let parseSegment = str => {
  let repeat = (t, n) => Some(Rationale.RList.repeat(t, int_of_string(n)));
  let op = Js.String.slice(~from=0, ~to_=1, str);
  let v = Js.String.sliceToEnd(~from=1, str);

  switch (op, v) {
  | ("U", i) => repeat((0, 1), i)
  | ("D", i) => repeat((0, (-1)), i)
  | ("L", i) => repeat(((-1), 0), i)
  | ("R", i) => repeat((1, 0), i)
  | _ => None
  };
};

let parsePath = x =>
  x
  ->split(",")
  ->Belt.List.fromArray
  ->Belt.List.map(parseSegment)
  ->combine
  ->Belt.Option.getWithDefault([])
  ->Belt.List.flatten;

let step = (a, b) =>
  a > b ?
    Rationale.RList.rangeInt(1, b, a) : Rationale.RList.rangeInt(1, a, b);

let coordinates = segments => {
  let map = Belt.Map.make(~id=(module T));
  let (_, _, steps) =
    Belt.List.reduce(
      segments,
      ((0, 0), 1, map),
      (((x0, y0), steps, map), (x1, y1)) => {
        let next = (x0 + x1, y0 + y1);
        Belt.Map.has(map, next) ?
          (next, steps + 1, map) :
          (next, steps + 1, Belt.Map.set(map, next, steps));
      },
    );

  steps;
};

let closest = (m1, m2) => {
  let p1 = Belt.Set.fromArray(Belt.Map.keysToArray(m1), ~id=(module T));
  let p2 = Belt.Set.fromArray(Belt.Map.keysToArray(m2), ~id=(module T));

  let get = (m, k) => Belt.Map.get(m, k)->Belt.Option.getWithDefault(0);

  Belt.Set.intersect(p1, p2)
  ->Belt.Set.toList
  ->Belt.List.map(p => get(m1, p) + get(m2, p))
  ->Belt.List.reduce(max_int, (current, d) => d < current ? d : current);
};

let run = input => {
  let paths =
    Belt.List.(
      input->split("\n")->fromArray->map(parsePath)->map(coordinates)
    );

  switch (paths) {
  | [a, b] => closest(a, b)
  | _ => 0
  };
};

Js.Console.log(run(Input.input));