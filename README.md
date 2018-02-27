# Synopsis

ppx\_binary creates binary serialization and deserialization functions for OCaml
record types. It allows users to specify the exact byte size and endianness
corresponding to a given record field.

Relative to libraries like Jane Street's
[bin_prot](https://github.com/janestreet/bin_prot/), ppx\_binary handles a much
smaller range of data types, but provides more specific control over how data is
represented on disk. It is best suited for cases where the serialization format
is determined by a pre-existing standard. For example, when working with BMP
files, data must be serialized and deserialized in the exact layout expected by
other image software. ppx\_binary allows this format to be expressed as an OCaml
record type, and automatically generates the corresponding serialization and
deserialization functions.


# Usage

The examples in this section draw on the
[BITMAPFILEHEADER](https://msdn.microsoft.com/en-us/library/windows/desktop/dd183374(v=vs.85).aspx)
and
[BITMAPINFOHEADER](https://msdn.microsoft.com/en-us/library/windows/desktop/dd183376(v=vs.85).aspx)
examples documented on MSDN.

## Basic types

### Stdint

ppx\_binary's records are built out of the integer types provided by the
[Stdint](http://stdint.forge.ocamlcore.org/doc/) library. Given a record with
fields of these types, we simply add the annotation `[@@deriving binary]` with
an argument specifying the endianness of the integers:

```ocaml
open Stdint

type t =
  { bfType: uint16
  ; bfSize: uint32
  ; bfReserved1: uint16
  ; bfReserved2: uint16
  ; bfOffBits: uint32 }
  [@@deriving binary ~endianness:"little"]
```

From this declaration, ppx\_binary will generate the following values:

* `of_bytes : Bytes.t -> int -> t`, where `of_bytes buffer n` reads a `t` from
  `buffer` starting at offset `n`;

* `to_bytes : t -> Bytes.t -> int -> unit`, where `to_bytes t buffer n` writes
  `t` to `buffer` starting at `int`;
  
* `byte_size : int`, the size of a serialized `t` in bytes.

(If this type had any name other than `t`, such as `foo`, the resulting values
would be named `foo_of_bytes`, `foo_to_bytes`, and `byte_size_foo`.)

### Masking with `int`

Unfortunately, OCaml's lack of typeclasses means that there are different
functions corresponding to each different type in the Stdint library. If you
want to multiply a `uint16` by 2, you will need to perform explicit conversions
such as `UInt16.(n * (of_int 2))`.

As an alternative, ppx\_binary can be told to automatically convert to and from
OCaml's built-in `int` type:

```ocaml
type t =
  { bfType: int [@masking uint16]
  ; bfSize: int [@masking uint32]
  ; bfReserved1: int [@masking uint16]
  ; bfReserved2: int [@masking uint16]
  ; bfOffBits: int [@masking uint32] }
  [@@deriving binary ~endianness:"little"]
```

The resulting record fields are all of type `int`, and so can be manipulated
without explicit conversion. The generated code automatically handles conversion
upon serialization and deserialization.

The downside of this approach is that ppx\_binary does not verify that the `int`
type on a given platform can accomodate all values of a given Stdint type. For
example, converting an `int64` to an `int` can have unintended results on a
32-bit platform. The user is responsible for ensuring that only safe conversions
are attempted.

## Compound types

ppx\_binary can work with other record field types provided that there are
appropriate `of_bytes`, `to_bytes`, and `byte_size` values defined. In the
following example, implementations of the BITMAPFILEHEADER and BITMAPINFOHEADER
structures are combined into one record that encapsulates the complete BMP
header:

```ocaml
module Bitmapfileheader = struct
  type t =
    { bfType: int [@masking uint16]
    ; bfSize: int [@masking uint32]
    ; bfReserved1: int [@masking uint16]
    ; bfReserved2: int [@masking uint16]
    ; bfOffBits: int [@masking uint32] }
    [@@deriving binary ~endianness:"little"]
end

module Bitmapinfoheader = struct
  type t =
    { biSize: int [@masking uint32]
    ; biWidth: int [@masking int32]
    ; biHeight: int [@masking int32]
    ; biPlanes: int [@masking uint16]
    ; biBitCount: int [@masking uint16]
    ; biCompression: int [@masking uint32]
    ; biSizeImage: int [@masking uint32]
    ; biXPelsPerMeter: int [@masking int32]
    ; biYPelsPerMeter: int [@masking int32]
    ; biClrUsed: int [@masking uint32]
    ; biClrImportant: int [@masking uint32] }
  [@@deriving binary ~endianness:"little"]
end

module Header = struct
  type t =
    { fileheader : Bitmapfileheader.t
    ; infoheader : Bitmapinfoheader.t }
  [@@deriving binary]
end
```

In the declaration of `Header.t`, we do not need to provide an endianness
because we are not directly using any integer types. If we needed to specify
endianness for a specific field, this could be done by adding an `[@endianness
little]` or `[@endianness big]` attribute on that field.

This example can be seen in action in `examples/resize.ml` and
`examples/resize_masking.ml`.
