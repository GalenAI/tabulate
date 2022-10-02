open Core

type t =
  | Drug_to_drug of { first : string; second : string }
  | Drug_allergy of string
  | Wrong_side
  | Wrong_site of { actual : string; wrong : string }
[@@deriving sexp, compare, yojson]

include Comparable.S with type t := t
