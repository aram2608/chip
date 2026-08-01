type mode = IABC | IvABC | IABx | IAsBx | IAx | IsJ [@@deriving enum]

(* We only have 31 bits, 1 of them is used by OCaml for the garbage collector *)
module OpMetadata = struct
  type t = int

  (* Mask Offsets *)
  let mode_mask = 0x07
  let sets_a = 0x08
  let is_test = 0x10
  let uses_it = 0x20
  let uses_ot = 0x40
  let uses_mm = 0x80

  (* Getters *)
  let mode metadata = mode_of_enum (metadata land mode_mask)
  let has_sets_a m = m land sets_a <> 0
  let has_is_test m = m land is_test <> 0
  let has_uses_it m = m land uses_it <> 0
  let has_uses_ot m = m land uses_ot <> 0
  let has_uses_mm m = m land uses_mm <> 0

  (* Builder *)
  let create mode sa test ?(it = false) ?(ot = false) ?(mm = false) () =
    mode_to_enum mode land mode_mask
    lor (if sa then sets_a else 0)
    lor (if test then is_test else 0)
    lor (if it then uses_it else 0)
    lor (if ot then uses_ot else 0)
    lor if mm then uses_mm else 0
end

type op =
  | Move
  | LoadK
  | Add
  | Sub
  | Mult
  | Div
  | Ret
  | Test
  | Jmp
  | LT
  | LE
  | GT
  | GE
  | EQ
  | NE
  | BAnd
  | BOr
  | BXor
  | BNot
  | CallB
  | Call
  | GGet
  | GSet
  | Print
[@@deriving show, enum]

let op_info =
  [|
    (* Move  *) OpMetadata.create IABC true false ~ot:true ();
    (* LoadK *) OpMetadata.create IABx true false ~ot:true ();
    (* Add   *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* Sub   *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* Mult  *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* Div   *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* Ret   *) OpMetadata.create IABC false false ();
    (* Test  *) OpMetadata.create IABC false true ();
    (* Jmp   *) OpMetadata.create IsJ false false ();
    (* LT    *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* LE    *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* GT    *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* GE    *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* EQ    *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* NE    *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* BAnd  *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* BOr   *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* BXor  *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* BNot  *) OpMetadata.create IABC true false ~it:true ~ot:true ~mm:true ();
    (* CallB *) OpMetadata.create IABC true false ~mm:true ();
    (* Call  *) OpMetadata.create IABC true false ~mm:true ();
    (* GGet  *) OpMetadata.create IABx true false ~ot:true ();
    (* GSet  *) OpMetadata.create IABx false false ~it:true ();
    (* Print *) OpMetadata.create IABC false false ();
  |]

let size_c = 8
let size_vc = 10
let size_b = 8
let size_vb = 6
let size_bx = size_b + size_c + 1
let size_a = 8
let op_size = 7
let size_ax = size_bx + size_a
let size_sj = size_bx + size_a
let pos_op = 0
let pos_a = pos_op + op_size
let pos_k = pos_a + size_a
let pos_b = pos_k + 1
let pos_vb = pos_k + 1
let pos_c = pos_b + size_b
let pos_vc = pos_vb + size_vb
let pos_bx = pos_k
let pos_ax = pos_a
let pos_sj = pos_a

(* limits *)
let max_a = (1 lsl size_a) - 1
let max_b = (1 lsl size_b) - 1
let max_vb = (1 lsl size_vb) - 1
let max_c = (1 lsl size_c) - 1
let max_vc = (1 lsl size_vc) - 1
let max_bx = (1 lsl size_bx) - 1
let max_ax = (1 lsl size_ax) - 1
let max_bx = (1 lsl size_bx) - 1
let max_sj = (1 lsl size_sj) - 1
let off_c = max_c lsr 1
let off_bx = max_bx lsr 1
let off_sj = max_sj lsr 1
let max_regs = max_a

module BitUtils = struct
  let mask1 n p = lnot (lnot 0 lsl n) lsl p
  let mask0 n p = lnot (mask1 n p)
  let get_arg inst pos sz = (inst lsr pos) land mask1 sz 0

  let set_arg inst pos sz v =
    let cleared = inst land mask0 sz pos in
    cleared lor ((v lsl pos) land mask1 sz pos)
end

let get_opcode i = i land ((1 lsl op_size) - 1)
let get_a i = BitUtils.get_arg i pos_a size_a
let get_b i = BitUtils.get_arg i pos_b size_b
let get_c i = BitUtils.get_arg i pos_c size_c
let get_bx i = BitUtils.get_arg i pos_bx size_bx
let get_k i = (i lsr pos_k) land 1
let get_sj i = BitUtils.get_arg i pos_sj size_sj - off_sj

let create_ABCk op a b c k =
  assert (a <= max_a && b <= max_b && c <= max_c && k <= 1);
  let o = op_to_enum op lsl pos_op in
  o lor (a lsl pos_a) lor (b lsl pos_b) lor (c lsl pos_c) lor (k lsl pos_k)

let create_ABx op a bx =
  assert (a <= max_a && bx <= max_bx);
  let o = op_to_enum op lsl pos_op in
  o lor (a lsl pos_a) lor (bx lsl pos_bx)

let create_sj op sj k =
  assert (sj >= -off_sj && sj <= max_sj - off_sj && k <= 1);
  let o = op_to_enum op in
  o lor ((sj + off_sj) lsl pos_sj) lor (k lsl pos_k)
