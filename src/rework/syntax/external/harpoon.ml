open Support
open Common

type defer_kind =
  [ `subgoal
  | `theorem
  ]

type invoke_kind =
  [ `ih
  | `lemma
  ]

type split_kind =
  [ `split
  | `invert
  | `impossible
  ]

type level =
  [ `meta
  | `comp
  ]

type automation_kind =
  [ `auto_intros
  | `auto_solve_trivial
  ]

type automation_change =
  [ `on
  | `off
  | `toggle
  ]

type basic_command =
  [ `list
  | `defer
  ]

type info_kind = [ `prog ]

type command =
  (* Administrative commands *)
  | Rename of
      { rename_from : Name.t
      ; rename_to : Name.t
      ; level : level
      }
  | ToggleAutomation of automation_kind * automation_change
  | Type of Comp.exp_syn
  | Info of info_kind * Name.t
  | SelectTheorem of Name.t
  | Theorem of
      [ basic_command | `show_ihs | `show_proof | `dump_proof of string ]
  | Session of [ basic_command | `create | `serialize ]
  | Subgoal of basic_command
  | Undo
  | Redo
  | History
  | Translate of Name.t
  (* Actual tactics *)
  | Intros of string list option (* list of names for introduced variables *)
  | Split of split_kind * Comp.exp_syn (* the expression to split on *)
  | MSplit of Location.t * Name.t (* split on a metavariable *)
  | Solve of Comp.exp_chk
  (* the expression to solve the current subgoal with *)
  | Unbox of Comp.exp_syn * Name.t * Comp.unbox_modifier option
  | By of Comp.exp_syn * Name.t
  | Suffices of Comp.exp_syn * Comp.suffices_typ list
  | Help
