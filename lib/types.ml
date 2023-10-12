type typ =
  | TInt
  | TChar
  | TBool
  | TIntList
  | TCharList
  | TBoolList
  | TUnknownList
  | TNone
  | TUnknown
  | TIntF
  | TCharF
  | TBoolF
  | TIntListF
  | TCharListF
  | TBoolListF
  | TUnknownListF
  | TNoneF
  | TUnknownF
  | TError
  | TUndef

let string_of_typ = function
  | TInt -> "int"
  | TChar -> "char"
  | TBool -> "bool"
  | TIntList -> "intlist"
  | TCharList -> "charlist"
  | TBoolList -> "boollist"
  | TNone -> "none"
  | TUnknown -> "unknown"
  | TUnknownList -> "unknownlist"
  | TIntF -> "int"
  | TCharF -> "char"
  | TBoolF -> "bool"
  | TIntListF -> "intlist"
  | TCharListF -> "charlist"
  | TBoolListF -> "boollist"
  | TNoneF -> "none"
  | TUnknownF -> "unknown"
  | TUnknownListF -> "unknown"
  | TError -> "error"
  | TUndef -> "undefined"

let funtyp_of_typ = function
  | TInt -> TIntF
  | TChar -> TCharF
  | TBool -> TBoolF
  | TIntList -> TIntListF
  | TCharList -> TCharListF
  | TBoolList -> TBoolListF
  | TNone -> TNoneF
  | TUnknown -> TUnknownF
  | TUnknownList -> TUnknownListF
  | _ -> failwith "Precondition failed"

let list_of_typ = function
  | TInt -> TIntList
  | TChar -> TCharList
  | TBool -> TBoolList
  | TUnknown -> TUnknownList
  | _ -> failwith "Precondition failed"

let typ_of_list = function
  | TIntList -> TInt
  | TBoolList -> TBool
  | TCharList -> TChar
  | _ -> failwith "Precondition failed"

let is_fun = function
  | TIntF | TCharF | TBoolF | TIntListF | TCharListF | TBoolListF | TNoneF
  | TUnknownF ->
      true
  | _ -> false

let is_list = function
  | TIntList | TCharList | TBoolList | TUnknownList -> true
  | _ -> false
