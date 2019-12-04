type operation =
  | Add(int, int, int)
  | Mult(int, int, int)
  | Error
  | Return;

type evaluation('a) =
  | Continue('a)
  | Error
  | Result('a);

let write = (env, reg, v) => Rationale.RList.update(v, reg, env);
let add = (a, b) => a + b;
let multi = (a, b) => a * b;
let lookup = (env, n) => Rationale.RList.nth(n, env);
let toOps = Rationale.RList.splitEvery(4);
let nth = Rationale.RList.nth;

let parseLine = l =>
  switch (l) {
  | [1, a, b, dst] => Add(a, b, dst)
  | [2, a, b, dst] => Mult(a, b, dst)
  | [99] => Return
  | [99, _] => Return
  | [99, _, _] => Return
  | [99, _, _, _] => Return
  | x =>
    Js.Console.log2("Parse error:", x);
    Error;
  };

let apply = (f, a, b, dst, env) =>
  Rationale.Option.Infix.(
    Rationale.Option.pure(f)
    <*> lookup(env, a)
    <*> lookup(env, b)
    <$> write(env, dst, _)
    <$> (x => Continue(x))
  )
  ->Rationale.Option.default(Error, _);

let eval = (op, env) =>
  switch (op) {
  | Add(a, b, dst) => apply(add, a, b, dst, env)
  | Mult(a, b, dst) => apply(multi, a, b, dst, env)
  | Error => Error
  | Return => Result(env)
  };

let run = input => {
  let rec doRun = (env, pc) => {
    let result =
      Rationale.Option.Infix.(
        (env->toOps->nth(pc, _) <$> parseLine <$> eval(_, env))
        ->Rationale.Option.default(Error, _)
      );

    switch (result) {
    | Continue(env) => doRun(env, pc + 1)
    | Result(env) => Some(env)
    | Error => None
    };
  };

  doRun(input, 0);
};

let range = Rationale.RList.rangeInt(1, 1, _);

let res =
  List.map(i => List.map(j => (i, j), range(99)), range(99))
  ->List.flatten
  ->List.find(
      ((i, j)) => {
        let res =
          Rationale.Option.Infix.(
            Input.input->write(1, i)->write(2, j)->run >>= nth(0)
          );

        switch (res) {
        | Some(19690720) => true
        | _ => false
        };
      },
      _,
    );

Js.Console.log(res);

/* Js.Console.log(run(Input.input)); */