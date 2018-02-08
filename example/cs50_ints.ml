(* An OCaml inspiration of CS50's "Resize" problem set, the original inspiration
   for this project.

   This version represents all data using OCaml's basic int type, avoiding the
   need for tedious explicit conversions when operating on the data. *)
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
  module T = struct
    type t =
      { fileheader : Bitmapfileheader.t
      ; infoheader : Bitmapinfoheader.t }
    [@@deriving binary]
  end

  include T
  include RW (T)
end

module Pixel = struct
  module T = struct
    type t =
      { rgbtBlue: int [@masking uint8]
      ; rgbtGreen: int [@masking uint8]
      ; rgbtRed: int [@masking uint8] }
      [@@deriving binary ~endianness:"little"]
  end

  include T
  include RW (T)
end

let padding width = (4 - width * Pixel.byte_size mod 4) mod 4

let size factor infoheader =
  let open Bitmapinfoheader in
  let new_width, new_height =
    (factor * infoheader.biWidth, factor * infoheader.biHeight)
  in
  let new_padding = padding new_width in
  let pixel_size = Pixel.byte_size in
  let new_size =
    (new_width * pixel_size + new_padding) * (abs @@ new_height * pixel_size)
  in
  ( { infoheader with
      biWidth= new_width; biHeight= new_height; biSizeImage= new_size }
  , new_padding )


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
  let header = Header.of_channel ic in
  let old_padding = padding header.infoheader.biWidth in
  let new_infoheader, new_padding = size factor header.infoheader in
  let new_fileheader =
    { header.fileheader with
      bfSize=
        new_infoheader.biSizeImage +
          Bitmapfileheader.byte_size + Bitmapinfoheader.byte_size }
  in
  let new_header = Header.{fileheader = new_fileheader;
                           infoheader = new_infoheader} in
  let new_padding = padding new_infoheader.biWidth in
  Header.to_channel oc new_header ;
  for _ = 1 to abs header.infoheader.biHeight do
    let line =
      read_scanline ic header.infoheader.biWidth old_padding
    in
    let new_line = expand_list line factor in
    for _ = 1 to factor do write_scanline oc new_line new_padding done
  done


let () =
  let ic, oc, factor =
    (open_in Sys.argv.(1), open_out Sys.argv.(2), int_of_string Sys.argv.(3))
  in
  resize factor ic oc
