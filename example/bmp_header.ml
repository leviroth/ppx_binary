open Stdint

type t = {
  bfType: uint16;
  bfSize: uint32;
  bfReserved1 : uint16;
  bfReserved2 : uint16;
  bfOffBits : uint32;
} [@@deriving binary]

let () =
  let f = open_in_bin "smiley.bmp" in
  let buf = Bytes.create 14 in
  match input f buf 0 14 with
  | 14 -> let my_t = of_bytes buf 0 in
    print_endline @@ Printf.sprintf "bfType: %s" @@ Uint16.to_string_hex my_t.bfType
  | _ -> assert false
