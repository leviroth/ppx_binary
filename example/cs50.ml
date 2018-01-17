(* An OCaml inspiration of CS50's "Resize" problem set, the original inspiration
   for this project. *)
open Stdint

(** This functor adds of_channel and to_channel functions to make it easier to
   work with channels directly, instead of having to worry about creating
   buffers. *)
module RW (Base : sig
  type t

  val byte_size : int

  val of_bytes : Bytes.t -> int -> t

  val to_bytes : t -> Bytes.t -> int -> unit
end) =
struct
  let of_channel ic =
    let buf = Bytes.create Base.byte_size in
    really_input ic buf 0 Base.byte_size ;
    Base.of_bytes buf 0


  let to_channel oc t =
    let buf = Bytes.create Base.byte_size in
    Base.to_bytes t buf 0 ; output_bytes oc buf
end

module Bitmapfileheader = struct
  module T = struct
    type t =
      { bfType: uint16
      ; bfSize: uint32
      ; bfReserved1: uint16
      ; bfReserved2: uint16
      ; bfOffBits: uint32 }
      [@@deriving binary ~endianness:"little"]
  end

  include T
  include RW (T)
end

module Bitmapinfoheader = struct
  module T = struct
    type t =
      { biSize: uint32
      ; biWidth: int32
      ; biHeight: int32
      ; biPlanes: uint16
      ; biBitCount: uint16
      ; biCompression: uint32
      ; biSizeImage: uint32
      ; biXPelsPerMeter: int32
      ; biYPelsPerMeter: int32
      ; biClrUsed: uint32
      ; biClrImportant: uint32 }
      [@@deriving binary ~endianness:"little"]
  end

  include T
  include RW (T)
end

module Pixel = struct
  module T = struct
    type t =
      {rgbtBlue: uint8; rgbtGreen: uint8; rgbtRed: uint8}
      [@@deriving binary ~endianness:"little"]
  end

  include T
  include RW (T)
end

let padding width = (4 - width * Pixel.byte_size mod 4) mod 4

let size factor infoheader =
  let open Bitmapinfoheader in
  let open Int32 in
  let factor = of_int factor in
  let new_width, new_height =
    (factor * infoheader.biWidth, factor * infoheader.biHeight)
  in
  let new_padding = of_int @@ padding @@ to_int new_width in
  let pixel_size = of_int Pixel.byte_size in
  let new_size =
    Uint32.of_int32
    @@ (new_width * pixel_size + new_padding) * (abs @@ new_height * pixel_size)
  in
  ( { infoheader with
      biWidth= new_width; biHeight= new_height; biSizeImage= new_size }
  , to_int new_padding )


let add_size dword native_int = Uint32.(dword + of_int native_int)

let read_scanline ic width padding =
  let pixels = List.init width (fun _ -> Pixel.of_channel ic) in
  seek_in ic @@ pos_in ic + padding ;
  pixels


let expand_list l n =
  let rec add_n item n acc =
    match n with 0 -> acc | n -> add_n item (n - 1) (item :: acc)
  in
  List.flatten @@ List.map (fun item -> add_n item n []) l


let write_scanline oc line padding =
  List.iter (Pixel.to_channel oc) line ;
  seek_out oc @@ pos_out oc + padding


let resize factor ic oc =
  let fileheader = Bitmapfileheader.of_channel ic in
  let infoheader = Bitmapinfoheader.of_channel ic in
  let old_padding = padding @@ Int32.to_int infoheader.biWidth in
  let new_infoheader, new_padding = size factor infoheader in
  let new_fileheader =
    { fileheader with
      bfSize=
        List.fold_left add_size new_infoheader.biSizeImage
          [Bitmapfileheader.byte_size; Bitmapinfoheader.byte_size] }
  in
  let new_padding = padding @@ Int32.to_int new_infoheader.biWidth in
  Bitmapfileheader.to_channel oc new_fileheader ;
  Bitmapinfoheader.to_channel oc new_infoheader ;
  for _ = 1 to abs @@ Int32.to_int infoheader.biHeight do
    let line =
      read_scanline ic (Int32.to_int infoheader.biWidth) old_padding
    in
    let new_line = expand_list line factor in
    for _ = 1 to factor do write_scanline oc new_line new_padding done
  done


let () =
  let ic, oc, factor =
    (open_in Sys.argv.(1), open_out Sys.argv.(2), int_of_string Sys.argv.(3))
  in
  resize factor ic oc
