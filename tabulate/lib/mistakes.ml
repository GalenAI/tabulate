open Core
open Async

module T = struct
  type t =
    | Drug_to_drug of { first : string; second : string }
    | Drug_allergy of string
    | Wrong_side
    | Wrong_site of { actual : string; wrong : string }
  [@@deriving sexp, compare, yojson]
end

include T
include Comparable.Make (T)

let%expect_test "empty list" =
  [%yojson_of: t list] [] |> Yojson.Safe.to_string |> print_endline;
  [%expect {| [] |}];
  Deferred.unit

let%expect_test "all possible" =
  [%yojson_of: t list]
    [
      Drug_to_drug { first = "a"; second = "b" };
      Drug_allergy "test";
      Wrong_side;
      Wrong_site { actual = "a"; wrong = "b" };
    ]
  |> Yojson.Safe.to_string |> print_endline;
  [%expect
    {| [["Drug_to_drug",{"first":"a","second":"b"}],["Drug_allergy","test"],["Wrong_side"],["Wrong_site",{"actual":"a","wrong":"b"}]] |}];
  Deferred.unit
