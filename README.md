This (work in progress) ppx rewriter allows you to create record types for
working with binary file formats. For example, a BMP file header has a structure
specified
[here](https://msdn.microsoft.com/en-us/library/windows/desktop/dd183374(v=vs.85).aspx).
Using ppx_binary, we can create a reader function for this structure in ocaml:

```ocaml
open Stdint

type t = {
  bfType: uint16 [@endian little];
  bfSize: uint32 [@endian little];
  bfReserved1 : uint16 [@endian little];
  bfReserved2 : uint16 [@endian little];
  bfOffBits : uint32 [@endian little];
} [@@deriving binary]
```

This example produces a function `of_bytes : Bytes.t -> int -> t` that can be used
as follows:

```ocaml
let () =
  let f = open_in_bin "smiley.bmp" in
  let buf = Bytes.create 14 in
  let _ = input f buf 0 14 in
  let my_t = of_bytes buf 0 in
  print_endline @@ Printf.sprintf "bfType: %s" @@ Uint16.to_string_hex my_t.bfType
```
