type t = [%binary [a, int;
                   b, char]]

let () =
  let foo = {a = 3;
             b = 'c'}
  in print_int foo.a
