open Stdint

type t = {
  bfType: uint16;
  bfSize: uint32;
  bfReserved1 : uint16;
  bfReserved2 : uint16;
  bfOffBits : uint32;
}

let read : Bytes.t -> int -> t =
  fun buf offset ->
    let bfType = Uint16.of_bytes_little_endian buf offset in
    let bfSize = Uint32.of_bytes_little_endian buf (offset + 2) in
    let bfReserved1 = Uint16.of_bytes_little_endian buf (offset + 6) in
    let bfReserved2 = Uint16.of_bytes_little_endian buf (offset + 8) in
    let bfOffBits = Uint32.of_bytes_little_endian buf (offset + 10) in
    { bfType; bfSize; bfReserved1; bfReserved2; bfOffBits; }

let () =
  let f = open_in_bin "smiley.bmp" in
  let buf = Bytes.create 14 in
  match input f buf 0 14 with
  | 14 -> let my_t = read buf 0 in
    print_endline @@ Uint16.to_string_hex my_t.bfType
  | _ -> assert false
