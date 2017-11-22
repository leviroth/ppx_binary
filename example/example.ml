open Stdint

type t =
  { a: int32 [@endian little]
  ; b: uint8 [@endian little]
  ; c: int16 [@endian little] }
  [@@deriving binary]

let () =
  let buf = Bytes.of_string "fibjj12" in
  let x = of_bytes buf 0 in
  print_endline @@ Uint8.to_string x.b

