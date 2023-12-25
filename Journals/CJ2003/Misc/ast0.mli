(* AST for ASN.1 X.680 without modules *)

type tag_default = [`EXPLICIT_TAGS | `IMPLICIT_TAGS | `AUTOMATIC_TAGS]

val default_tagging : tag_default option

type tag_class = [`UNIVERSAL | `APPLICATION | `PRIVATE | `Context]

type tag_num = [`Pos of int | `VRef of string]

type tag_attr = [`IMPLICIT | `EXPLICIT | `Inferred]

type tag = [`Tag of tag_class * tag_num * tag_attr]

type restricted_string = [
  `Numeric | `Printable | `Teletex | `T61 | `Videotex | `IA5
| `Graphic | `Visible | `ISO646 | `General | `BMP
]

type identifier = string
type name = identifier
type label = identifier

type index = [`Pos of int | `Neg of int | `VRef of string]

type oid_component = [
  `Ident      of string
| `Pos        of int 
| `NameAndNum of identifier * [`Pos of int | `VRef of string]
]

type kind = Partial | Full

type in_out = [`Lt | `Let]

type presence = [`PRESENT | `ABSENT | `OPTIONAL]

type value_term = [
  `Ident    of string
| `Pos      of int
| `Neg      of int
| `Real     of float
| `HexStr   of string
| `BinStr   of string
| `List     of value_term list
| `AssoList of (identifier * value_term) list
| `ObjId    of oid_component list
| `Choice   of label * value_term
| `EmptyList
| `Null
| `PLUS_INFINITY
| `MINUS_INFINITY
| `TRUE
| `FALSE
] 

type bound = [`MIN | `MAX | `Value of value_term]

type type_term = [
  `CHOICE      of field list
| `SET         of component list
| `SEQUENCE    of component list
| `SET_OF      of subtype
| `SEQUENCE_OF of subtype
| `Selection   of field
| `INTEGER     of (name * index) list
| `BIT_STRING  of (name * index) list
| `ENUMERATED  of (name * index option) list
| `RestrStr    of restricted_string
| `TRef        of string
| `REAL
| `BOOLEAN
| `NULL
| `OCTET_STRING
| `OBJECT_IDENTIFIER
| `RELATIVE_OID 
| `EMBEDDED_PDV
| `EXTERNAL
| `CHARACTER_STRING
]

and subtype = tag list * type_term * type_constr option

and field = label * subtype

and component = [
  `Comp          of field * [`OPTIONAL | `DEFAULT of value_term] option
| `COMPONENTS_OF of subtype
] 

and type_constr = [
  `INTERSECTION    of type_constr
| `UNION           of type_constr * type_constr
| `EXCEPT          of type_constr * type_constr
| `ALL_EXCEPT      of type_constr
| `FROM            of type_constr
| `SIZE            of type_constr
| `WITH_COMPONENT  of type_constr
| `INCLUDES        of subtype
| `WITH_COMPONENTS of kind * field_constr list
| `Interval        of bound * in_out * in_out * bound
| `Value           of value_term
| `PATTERN         of value_term
] 

and field_constr = label * type_constr option * presence option

