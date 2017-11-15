open Stdint

type t = {a : int32;
          b : uint8;
          c : int16 } [@@deriving binary]

let () =
  let buf = Bytes.of_string "fibjj12" in
  let x = read_t buf 0 in
  print_endline @@ Uint8.to_string x.b