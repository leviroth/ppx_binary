open Stdint

module M : sig
  type t = {a: int32; b: uint8; c: int16} [@@deriving binary]
end = struct
  type t =
    {a: int32; b: uint8; c: int16}
    [@@deriving binary ~endianness:"little"]
end

let () =
  let buf = Bytes.of_string "fibjj12" in
  let x = M.of_bytes buf 0 in
  print_endline @@ Uint8.to_string x.b ;
  M.to_bytes x buf 0 ;
  print_endline @@ Bytes.to_string buf

