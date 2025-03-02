(* open Suplex.Syntax *)
open Suplex
open Suplex.Types

type r

let test_record_desc = empty_rec |+ field "a" i8 |+ field "b" i16

let test_record : r record typ = seal test_record_desc

let test_field_index () =
  Alcotest.(check unit)
    "test_field_index"
    ()
    begin
      (match field_index (field "a" i8) test_record with
      | Ok (Some 0) -> ()
      | Ok (Some i) ->
          Alcotest.failf "field_index failed: got index %d for .a" i
      | _ -> Alcotest.fail "field_index failed") ;

      (match field_index (field "b" i16) test_record with
      | Ok (Some 1) -> ()
      | Ok (Some i) ->
          Alcotest.failf "field_index failed: got index %d for .b" i
      | _ -> Alcotest.fail "field_index failed") ;

      (match field_index (field "a" i16) test_record with
      | Ok None -> ()
      | Ok (Some i) ->
          Alcotest.failf "field_index failed: got index %d for .a" i
      | Error msg -> Alcotest.failf "field_index failed (%s)" msg) ;

      match field_index (field "c" i8) test_record with
      | Ok None -> ()
      | Ok (Some i) ->
          Alcotest.failf "field_index failed: got index %d for .c" i
      | Error msg -> Alcotest.failf "field_index failed (%s)" msg
    end

let test_proj () =
  Alcotest.(check unit)
    "test_proj"
    ()
    begin
      let ((_, a), b) = projs test_record_desc in
      let (Field { name; ty }) = a in
      Alcotest.(check string) "test_proj" "a" name ;
      Alcotest.(check string) "test_proj" "i8" (Format.asprintf "%a" pp_typ ty) ;
      let (Field { name; ty }) = b in
      Alcotest.(check string) "test_proj" "b" name ;
      Alcotest.(check string) "test_proj" "i16" (Format.asprintf "%a" pp_typ ty)
    end

let () =
  let open Alcotest in
  run
    "types_test"
    [ ( "basic",
        [ test_case "test_field_index" `Quick test_field_index;
          test_case "test_proj" `Quick test_proj ] ) ]
