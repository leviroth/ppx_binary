open Base
open Stdint
open OUnit2

let data = "\x42\x4d\x5a\x00\x00\x00\x00\x00\x00\x00\x36\x00\x00\x00\x28\x00\x00\x00\x03\x00\x00\x00\xfd\xff\xff\xff\x01\x00\x18\x00\x00\x00\x00\x00\x24\x00\x00\x00\x12\x0b\x00\x00\x12\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x00\x00\xff\x00\x00\xff\x00\x00\x00\x00\x00\xff\x00\xff\xff\xff\x00\xff\x00\x00\x00\x00\x00\xff\x00\x00\xff\x00\x00\xff\x00\x00\x00\x00"

let buffer = Bytes.of_string data

module Test_no_masking = struct
  module Bitmapfileheader = struct
    type t =
      { bfType: uint16
      ; bfSize: uint32
      ; bfReserved1: uint16
      ; bfReserved2: uint16
      ; bfOffBits: uint32 }
    [@@deriving binary ~endianness:"little"]
  end

  module Bitmapinfoheader = struct
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

  module Header = struct
    module T = struct
      type t =
        { fileheader: Bitmapfileheader.t
        ; infoheader: Bitmapinfoheader.t }
      [@@deriving binary]
    end

    include T
  end

  let header = Header.of_bytes buffer 0
  let out_buffer = Bytes.create 90

  let test _ =
    assert_equal ~msg:"Byte size"
      Header.byte_size
      54;

    assert_equal ~msg:"Read bfSize"
      header.Header.fileheader.Bitmapfileheader.bfSize
      (Uint32.of_int 90);
    assert_equal ~msg:"Read bfHeight"
      header.Header.infoheader.Bitmapinfoheader.biHeight
      (Int32.of_int (-3));

    Header.to_bytes header out_buffer 0;
    for i = 0 to (Header.byte_size - 1) do
      let msg = Printf.sprintf "Byte %d matches" i in
      assert_equal ~msg
        (Bytes.get buffer i)
        (Bytes.get out_buffer i)
    done
end

module Test_masking = struct
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
        { fileheader: Bitmapfileheader.t
        ; infoheader: Bitmapinfoheader.t }
      [@@deriving binary]
    end

    include T
  end

  let header = Header.of_bytes buffer 0
  let out_buffer = Bytes.create 90

  let test _ =
    assert_equal ~msg:"Byte size"
      Header.byte_size
      54;

    assert_equal ~msg:"Read bfSize"
      header.Header.fileheader.Bitmapfileheader.bfSize
      90;
    assert_equal ~msg:"Read bfHeight"
      header.Header.infoheader.Bitmapinfoheader.biHeight
      (-3);

    Header.to_bytes header out_buffer 0;
    for i = 0 to (Header.byte_size - 1) do
      let msg = Printf.sprintf "Byte %d matches" i in
      assert_equal ~msg
        (Bytes.get buffer i)
        (Bytes.get out_buffer i)
    done
end

let suite =
  "suite" >::: [
    "test_no_masking" >:: Test_no_masking.test;
    "test_masking" >:: Test_masking.test;
  ]

let () =
  run_test_tt_main suite
