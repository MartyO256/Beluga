(** Pretty-printing for the external syntax. *)

open Support
open Synext'

(** {1 Handling Parentheses}

    The external syntax models parentheses explicitly. This allows for
    handling operator quoting, whereby infix and postfix operators may be
    written in prefix notation when the operator is parenthesized. Having
    parentheses modelled in the AST also allows for better error-reporting
    since there is a discrepancy between the location of an object and that
    same object in parentheses. Most importantly, having parentheses modelled
    in the AST allows for decoupling the elimination of redundant parentheses
    and the re-introduction of necessary parentheses from the printing
    functions. This enables finer testing.

    {2 Removing Parentheses}

    The syntax tree structure captures the grouping of terms without the need
    for parentheses. As part of pretty-printing for the external syntax, we
    can remove semantically irrelevant parentheses for normalization, and
    re-introduce them as needed when printing. As such, program generation
    from the internal syntax to the external syntax does not need to handle
    parenthesizing.

    {2 Adding Necessary Parentheses}

    Parentheses are re-introduced in the AST for printing using the
    precedence ordering specified in the parser. Careful thought is required
    when adding parentheses for user-defined operators, especially when
    operator precedences are equal.

    Let [->] be a right-associative infix operator and [<-] be a
    left-associative infix operator, both of the same precedence.

    Parentheses are required in the following cases:

    - [(a <- b) -> c]
    - [(a -> b) -> c]
    - [a <- (b -> c)]
    - [a <- (b <- c)]

    Parentheses are not required in the following cases:

    - [a -> b -> c], which is parsed as [a -> (b -> c)]
    - [a <- b <- c], which is parsed as [(a <- b) <- c]
    - [a -> b <- c], which is parsed as [(a -> b) <- c]
    - [a <- b -> c], which is parsed as [a <- (b -> c)] *)

(** {1 Printing LF Kinds, Types and Terms} *)
module LF = struct
  open LF

  (** {1 Removing Parentheses} *)

  (** [remove_parentheses_kind kind] is [kind] without parentheses, except
      for quoted type-level and term-level constants. Printing this kind with
      {!pp_kind} may not result in a syntactically valid LF kind. *)
  let rec remove_parentheses_kind kind =
    match kind with
    | Kind.Typ _ -> kind
    | Kind.Arrow { location; domain; range } ->
      let domain' = remove_parentheses_typ domain
      and range' = remove_parentheses_kind range in
      Kind.Arrow { location; domain = domain'; range = range' }
    | Kind.Pi { location; parameter_identifier; parameter_type; body } ->
      let parameter_type' = remove_parentheses_typ parameter_type
      and body' = remove_parentheses_kind body in
      Kind.Pi
        { location
        ; parameter_identifier
        ; parameter_type = parameter_type'
        ; body = body'
        }
    | Kind.Parenthesized { kind; _ } -> remove_parentheses_kind kind

  (** [remove_parentheses_typ typ] is [typ] without parentheses, except for
      quoted type-level and term-level constants. Printing this type with
      {!pp_typ} may not result in a syntactically valid LF type. *)
  and remove_parentheses_typ typ =
    match typ with
    | Typ.Constant _ -> typ
    | Typ.Application { location; applicand; arguments } ->
      let applicand' = remove_parentheses_typ applicand
      and arguments' = List.map remove_parentheses_term arguments in
      let applicand'' =
        match applicand' with
        | Typ.Parenthesized { typ = Typ.Constant { operator; _ } as c; _ }
          when Operator.is_prefix operator -> c
        | _ -> applicand'
      in
      Typ.Application
        { location; applicand = applicand''; arguments = arguments' }
    | Typ.ForwardArrow { location; domain; range } ->
      let domain' = remove_parentheses_typ domain
      and range' = remove_parentheses_typ range in
      Typ.ForwardArrow { location; domain = domain'; range = range' }
    | Typ.BackwardArrow { location; domain; range } ->
      let domain' = remove_parentheses_typ domain
      and range' = remove_parentheses_typ range in
      Typ.BackwardArrow { location; domain = domain'; range = range' }
    | Typ.Pi { location; parameter_identifier; parameter_type; body } ->
      let parameter_type' = remove_parentheses_typ parameter_type
      and body' = remove_parentheses_typ body in
      Typ.Pi
        { location
        ; parameter_identifier
        ; parameter_type = parameter_type'
        ; body = body'
        }
    | Typ.Parenthesized { typ = Typ.Constant _; _ } -> typ
    | Typ.Parenthesized { typ; _ } -> remove_parentheses_typ typ

  (** [remove_parentheses_term term] is [term] without parentheses, except
      for quoted type-level and term-level constants. Printing this term with
      {!pp_term} may not result in a syntactically valid LF term. *)
  and remove_parentheses_term term =
    match term with
    | Term.Variable _ -> term
    | Term.Constant _ -> term
    | Term.Application { location; applicand; arguments } ->
      let applicand' = remove_parentheses_term applicand
      and arguments' = List.map remove_parentheses_term arguments in
      let applicand'' =
        match applicand' with
        | Term.Parenthesized { term = Term.Constant { operator; _ } as c; _ }
          when Operator.is_prefix operator -> c
        | _ -> applicand'
      in
      Term.Application
        { location; applicand = applicand''; arguments = arguments' }
    | Term.Abstraction
        { location; parameter_identifier; parameter_type; body } ->
      let parameter_type' = Option.map remove_parentheses_typ parameter_type
      and body' = remove_parentheses_term body in
      Term.Abstraction
        { location
        ; parameter_identifier
        ; parameter_type = parameter_type'
        ; body = body'
        }
    | Term.Wildcard _ -> term
    | Term.TypeAnnotated { location; term; typ } ->
      let term' = remove_parentheses_term term
      and typ' = remove_parentheses_typ typ in
      Term.TypeAnnotated { location; term = term'; typ = typ' }
    | Term.Parenthesized { term; _ } -> remove_parentheses_term term

  (** {1 Adding Necessary Parentheses} *)

  let add_parentheses_kind kind =
    let location = location_of_kind kind in
    Kind.Parenthesized { location; kind }

  let add_parentheses_typ typ =
    let location = location_of_typ typ in
    Typ.Parenthesized { location; typ }

  let add_parentheses_term term =
    let location = location_of_term term in
    Term.Parenthesized { location; term }

  (** [parenthesize_kind kind] is [kind] with the addition of necessary
      parentheses for printing.

      If this is done after the application of {!remove_parentheses_kind},
      then the resultant LF kind can be parsed to the same AST modulo
      {!remove_parentheses_kind} and without considering locations. *)
  let rec parenthesize_kind kind =
    match kind with
    | Kind.Typ _ -> kind
    | Kind.Arrow { location; domain; range } ->
      let domain' =
        match domain with
        | Typ.Pi _ | Typ.ForwardArrow _ | Typ.BackwardArrow _ ->
          add_parentheses_typ (parenthesize_typ domain)
        | _ -> parenthesize_typ domain
      and range' = parenthesize_kind range in
      Kind.Arrow { location; domain = domain'; range = range' }
    | Kind.Pi { location; parameter_identifier; parameter_type; body } ->
      let parameter_type' = parenthesize_typ parameter_type
      and body' = parenthesize_kind body in
      Kind.Pi
        { location
        ; parameter_identifier
        ; parameter_type = parameter_type'
        ; body = body'
        }
    | Kind.Parenthesized _ -> kind

  (** [parenthesize_typ typ] is [typ] with the addition of necessary
      parentheses for printing.

      If this is done after the application of {!remove_parentheses_typ},
      then the resultant LF type can be parsed to the same AST modulo
      {!remove_parentheses_typ} and without considering locations. *)
  and parenthesize_typ typ =
    match typ with
    | Typ.Constant _ -> typ
    | Typ.Application
        { location
        ; applicand =
            Typ.Constant { operator = applicand_operator; _ } as applicand
        ; arguments
        } ->
      let arguments' =
        parenthesize_operator_application_arguments applicand_operator
          arguments
      in
      Typ.Application { location; applicand; arguments = arguments' }
    | Typ.Application { location; applicand; arguments } ->
      let applicand' =
        match applicand with
        | Typ.Application _
        | Typ.Pi _
        | Typ.ForwardArrow _
        | Typ.BackwardArrow _ ->
          add_parentheses_typ (parenthesize_typ applicand)
        | _ -> parenthesize_typ applicand
      in
      let arguments' = parenthesize_application_arguments arguments in
      Typ.Application
        { location; applicand = applicand'; arguments = arguments' }
    | Typ.ForwardArrow { location; domain; range } ->
      let domain' =
        match domain with
        | Typ.Pi _ | Typ.ForwardArrow _ | Typ.BackwardArrow _ ->
          add_parentheses_typ (parenthesize_typ domain)
        | _ -> parenthesize_typ domain
      and range' = parenthesize_typ range in
      Typ.ForwardArrow { location; domain = domain'; range = range' }
    | Typ.BackwardArrow { location; range; domain } ->
      let range' =
        match range with
        | Typ.Pi _ | Typ.ForwardArrow _ ->
          add_parentheses_typ (parenthesize_typ range)
        | _ -> parenthesize_typ range
      and domain' =
        match domain with
        | Typ.BackwardArrow _ ->
          add_parentheses_typ (parenthesize_typ domain)
        | _ -> parenthesize_typ domain
      in
      Typ.BackwardArrow { location; range = range'; domain = domain' }
    | Typ.Pi { location; parameter_identifier; parameter_type; body } ->
      let parameter_type' = parenthesize_typ parameter_type
      and body' = parenthesize_typ body in
      Typ.Pi
        { location
        ; parameter_identifier
        ; parameter_type = parameter_type'
        ; body = body'
        }
    | Typ.Parenthesized _ -> typ

  (** [parenthesize_term term] is [term] with the addition of necessary
      parentheses for printing.

      If this is done after the application of {!remove_parentheses_term},
      then the resultant LF term can be parsed to the same AST modulo
      {!remove_parentheses_term} and without considering locations. *)
  and parenthesize_term term =
    match term with
    | Term.Variable _ -> term
    | Term.Constant _ -> term
    | Term.Application
        { location
        ; applicand =
            Term.Constant { operator = applicand_operator; _ } as applicand
        ; arguments
        } ->
      let arguments' =
        parenthesize_operator_application_arguments applicand_operator
          arguments
      in
      Term.Application { location; applicand; arguments = arguments' }
    | Term.Application { location; applicand; arguments } ->
      let applicand' =
        match applicand with
        | Term.Application _ | Term.Abstraction _ | Term.TypeAnnotated _ ->
          add_parentheses_term (parenthesize_term applicand)
        | _ -> parenthesize_term applicand
      in
      let arguments' = parenthesize_application_arguments arguments in
      Term.Application
        { location; applicand = applicand'; arguments = arguments' }
    | Term.Abstraction
        { location; parameter_identifier; parameter_type; body } ->
      let parameter_type' = Option.map parenthesize_typ parameter_type
      and body' = parenthesize_term body in
      Term.Abstraction
        { location
        ; parameter_identifier
        ; parameter_type = parameter_type'
        ; body = body'
        }
    | Term.Wildcard _ -> term
    | Term.TypeAnnotated { location; term; typ } ->
      let term' =
        match term with
        | Term.Abstraction _ | Term.TypeAnnotated _ ->
          add_parentheses_term (parenthesize_term term)
        | _ -> parenthesize_term term
      and typ' = parenthesize_typ typ in
      Term.TypeAnnotated { location; term = term'; typ = typ' }
    | Term.Parenthesized _ -> term

  (** [parenthesize_application_arguments arguments] is the arguments in
      [argument] parenthesized as if they were applied to a non-operator
      applicand. *)
  and parenthesize_application_arguments arguments =
    List.map
      (fun argument ->
        match argument with
        | Term.Application _ | Term.Abstraction _ | Term.TypeAnnotated _ ->
          add_parentheses_term (parenthesize_term argument)
        | _ -> parenthesize_term argument)
      arguments

  (** [parenthesize_operator_application_arguments applicand_operator arguments]
      is the arguments in [arguments] parenthesized as if they were applied
      to an operator applicand described by [applicand_operator]. *)
  and parenthesize_operator_application_arguments applicand_operator
      arguments =
    match Operator.fixity applicand_operator with
    | Fixity.Prefix ->
      parenthesize_prefix_operator_application_arguments arguments
    | Fixity.Infix ->
      assert (List.length arguments = 2);
      let[@warning "-8"] [ left_argument; right_argument ] = arguments in
      parenthesize_infix_operator_application_arguments applicand_operator
        left_argument right_argument
    | Fixity.Postfix ->
      assert (List.length arguments = 1);
      let[@warning "-8"] [ argument ] = arguments in
      parenthesize_postfix_operator_application_arguments applicand_operator
        argument

  and parenthesize_prefix_operator_application_arguments arguments =
    parenthesize_application_arguments arguments

  and parenthesize_infix_operator_application_arguments applicand_operator
      left_argument right_argument =
    match Operator.associativity applicand_operator with
    | Associativity.Left_associative ->
      parenthesize_infix_left_associative_operator_application_arguments
        applicand_operator left_argument right_argument
    | Associativity.Right_associative ->
      parenthesize_infix_right_associative_operator_application_arguments
        applicand_operator left_argument right_argument
    | Associativity.Non_associative ->
      parenthesize_infix_non_associative_operator_application_arguments
        applicand_operator left_argument right_argument

  and parenthesize_infix_left_associative_operator_application_arguments
      applicand_operator left_argument right_argument =
    let left_argument' =
      match left_argument with
      | Term.Application
          { applicand =
              Term.Constant { operator = left_argument_operator; _ }
          ; _
          }
        when Bool.not
               (Operator.precedence applicand_operator
               > Operator.precedence left_argument_operator) ->
        parenthesize_term left_argument
      | Term.Application _ | Term.Abstraction _ | Term.TypeAnnotated _ ->
        add_parentheses_term (parenthesize_term left_argument)
      | _ -> parenthesize_term left_argument
    and right_argument' =
      match right_argument with
      | Term.Application
          { applicand =
              Term.Constant { operator = right_argument_operator; _ }
          ; _
          }
        when Bool.not
               (Operator.precedence applicand_operator
                >= Operator.precedence right_argument_operator
               && Operator.is_left_associative right_argument_operator) ->
        parenthesize_term right_argument
      | Term.Application _ | Term.Abstraction _ | Term.TypeAnnotated _ ->
        add_parentheses_term (parenthesize_term right_argument)
      | _ -> parenthesize_term right_argument
    in
    [ left_argument'; right_argument' ]

  and parenthesize_infix_right_associative_operator_application_arguments
      applicand_operator left_argument right_argument =
    let left_argument' =
      match left_argument with
      | Term.Application
          { applicand =
              Term.Constant { operator = left_argument_operator; _ }
          ; _
          }
        when Bool.not
               (Operator.precedence applicand_operator
               >= Operator.precedence left_argument_operator) ->
        parenthesize_term left_argument
      | Term.Application _ | Term.Abstraction _ | Term.TypeAnnotated _ ->
        add_parentheses_term (parenthesize_term left_argument)
      | _ -> parenthesize_term left_argument
    and right_argument' =
      match right_argument with
      | Term.Application
          { applicand =
              Term.Constant { operator = right_argument_operator; _ }
          ; _
          }
        when Bool.not
               (Operator.precedence applicand_operator
               > Operator.precedence right_argument_operator) ->
        parenthesize_term right_argument
      | Term.Application _ | Term.Abstraction _ | Term.TypeAnnotated _ ->
        add_parentheses_term (parenthesize_term right_argument)
      | _ -> parenthesize_term right_argument
    in
    [ left_argument'; right_argument' ]

  and parenthesize_infix_non_associative_operator_application_arguments
      applicand_operator left_argument right_argument =
    let left_argument' =
      match left_argument with
      | Term.Application
          { applicand =
              Term.Constant { operator = left_argument_operator; _ }
          ; _
          }
        when Bool.not
               (Operator.precedence applicand_operator
                > Operator.precedence left_argument_operator
               && Operator.is_non_associative left_argument_operator) ->
        parenthesize_term left_argument
      | Term.Application _ | Term.Abstraction _ | Term.TypeAnnotated _ ->
        add_parentheses_term (parenthesize_term left_argument)
      | _ -> parenthesize_term left_argument
    and right_argument' =
      match right_argument with
      | Term.Application
          { applicand =
              Term.Constant { operator = right_argument_operator; _ }
          ; _
          }
        when Bool.not
               (Operator.precedence applicand_operator
                > Operator.precedence right_argument_operator
               && Operator.is_non_associative right_argument_operator) ->
        parenthesize_term right_argument
      | Term.Application _ | Term.Abstraction _ | Term.TypeAnnotated _ ->
        add_parentheses_term (parenthesize_term right_argument)
      | _ -> parenthesize_term right_argument
    in
    [ left_argument'; right_argument' ]

  and parenthesize_postfix_operator_application_arguments applicand_operator
      argument =
    let argument' =
      match argument with
      | Term.Application
          { applicand = Term.Constant { operator = argument_operator; _ }
          ; _
          }
        when Bool.not
               (Operator.precedence applicand_operator
               >= Operator.precedence argument_operator)
             || Operator.is_postfix applicand_operator ->
        parenthesize_term argument
      | Term.Application _ | Term.Abstraction _ | Term.TypeAnnotated _ ->
        add_parentheses_term (parenthesize_term argument)
      | _ -> parenthesize_term argument
    in
    [ argument' ]

  (** {1 Printing} *)

  let rec pp_kind ppf kind =
    match kind with
    | Kind.Typ _ -> Format.fprintf ppf "type"
    | Kind.Arrow { domain; range; _ } ->
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]" pp_typ domain pp_kind range
    | Kind.Pi { parameter_identifier = Option.None; parameter_type; body; _ }
      ->
      Format.fprintf ppf "@[<2>{@ _@ :@ %a@ }@ %a@]" pp_typ parameter_type
        pp_kind body
    | Kind.Pi
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_type
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>{@ %a@ :@ %a@ }@ %a@]" Identifier.pp
        parameter_identifier pp_typ parameter_type pp_kind body
    | Kind.Parenthesized { kind; _ } ->
      Format.fprintf ppf "@[<2>(%a)@]" pp_kind kind

  and pp_typ ppf typ =
    match typ with
    | Typ.Constant { identifier; _ } ->
      Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
    | Typ.Application { applicand; arguments = []; _ } ->
      Format.fprintf ppf "@[<2>%a@]" pp_typ applicand
    | Typ.Application
        { applicand = Typ.Constant { identifier; operator; _ }
        ; arguments
        ; _
        } -> (
      match Operator.fixity operator with
      | Fixity.Prefix ->
        Format.fprintf ppf "@[<2>%a@ %a@]" QualifiedIdentifier.pp identifier
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_term)
          arguments
      | Fixity.Infix ->
        assert (List.length arguments = 2);
        let[@warning "-8"] [ left_argument; right_argument ] = arguments in
        Format.fprintf ppf "@[<2>%a@ %a@ %a@]" pp_term left_argument
          QualifiedIdentifier.pp identifier pp_term right_argument
      | Fixity.Postfix ->
        assert (List.length arguments = 1);
        let[@warning "-8"] [ argument ] = arguments in
        Format.fprintf ppf "@[<2>%a@ %a@]" pp_term argument
          QualifiedIdentifier.pp identifier)
    | Typ.Application { applicand; arguments; _ } ->
      Format.fprintf ppf "@[<2>%a@ %a@]" pp_typ applicand
        (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_term)
        arguments
    | Typ.ForwardArrow { domain; range; _ } ->
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]" pp_typ domain pp_typ range
    | Typ.BackwardArrow { range; domain; _ } ->
      Format.fprintf ppf "@[<2>%a@ <-@ %a@]" pp_typ range pp_typ domain
    | Typ.Pi { parameter_identifier = Option.None; parameter_type; body; _ }
      ->
      Format.fprintf ppf "@[<2>{@ _@ :@ %a@ }@ %a@]" pp_typ parameter_type
        pp_typ body
    | Typ.Pi
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_type
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>{@ %a@ :@ %a@ }@ %a@]" Identifier.pp
        parameter_identifier pp_typ parameter_type pp_typ body
    | Typ.Parenthesized { typ; _ } ->
      Format.fprintf ppf "@[<2>(%a)@]" pp_typ typ

  and pp_term ppf term =
    match term with
    | Term.Variable { identifier; _ } ->
      Format.fprintf ppf "%a" Identifier.pp identifier
    | Term.Constant { identifier; _ } ->
      Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
    | Term.Application { applicand; arguments = []; _ } ->
      Format.fprintf ppf "@[<2>%a@]" pp_term applicand
    | Term.Application
        { applicand = Term.Constant { identifier; operator; _ }
        ; arguments
        ; _
        } -> (
      match Operator.fixity operator with
      | Fixity.Prefix ->
        Format.fprintf ppf "@[<2>%a@ %a@]" QualifiedIdentifier.pp identifier
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_term)
          arguments
      | Fixity.Infix ->
        assert (List.length arguments = 2);
        let[@warning "-8"] [ left_argument; right_argument ] = arguments in
        Format.fprintf ppf "@[<2>%a@ %a@ %a@]" pp_term left_argument
          QualifiedIdentifier.pp identifier pp_term right_argument
      | Fixity.Postfix ->
        assert (List.length arguments = 1);
        let[@warning "-8"] [ argument ] = arguments in
        Format.fprintf ppf "@[<2>%a@ %a@]" pp_term argument
          QualifiedIdentifier.pp identifier)
    | Term.Application { applicand; arguments; _ } ->
      Format.fprintf ppf "@[<2>%a@ %a@]" pp_term applicand
        (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_term)
        arguments
    | Term.Abstraction
        { parameter_identifier = Option.None
        ; parameter_type = Option.None
        ; body
        ; _
        } -> Format.fprintf ppf "@[<2>\\_.@ %a@]" pp_term body
    | Term.Abstraction
        { parameter_identifier = Option.None
        ; parameter_type = Option.Some parameter_type
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>\\_:%a.@ %a@]" pp_typ parameter_type pp_term
        body
    | Term.Abstraction
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_type = Option.None
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>\\%a.@ %a@]" Identifier.pp
        parameter_identifier pp_term body
    | Term.Abstraction
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_type = Option.Some parameter_type
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>\\%a:%a.@ %a@]" Identifier.pp
        parameter_identifier pp_typ parameter_type pp_term body
    | Term.Wildcard _ -> Format.fprintf ppf "_"
    | Term.TypeAnnotated { term; typ; _ } ->
      Format.fprintf ppf "@[<2>%a@ :@ %a@]" pp_term term pp_typ typ
    | Term.Parenthesized { term; _ } ->
      Format.fprintf ppf "@[<2>(%a)@]" pp_term term

  module Debug = struct
    let rec pp_kind ppf kind =
      match kind with
      | Kind.Typ _ -> Format.fprintf ppf "type"
      | Kind.Arrow { domain; range; _ } ->
        Format.fprintf ppf "@[<2>KindArrow(@,%a@ ->@ %a)@]" pp_typ domain
          pp_kind range
      | Kind.Pi
          { parameter_identifier = Option.None; parameter_type; body; _ } ->
        Format.fprintf ppf "@[<2>KindPi(@,{@ _@ :@ %a@ }@ %a)@]" pp_typ
          parameter_type pp_kind body
      | Kind.Pi
          { parameter_identifier = Option.Some parameter_identifier
          ; parameter_type
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>KindPi(@,{@ %a@ :@ %a@ }@ %a)@]"
          Identifier.pp parameter_identifier pp_typ parameter_type pp_kind
          body
      | Kind.Parenthesized { kind; _ } ->
        Format.fprintf ppf "@[<2>KindParenthesized(@,%a)@]" pp_kind kind

    and pp_typ ppf typ =
      match typ with
      | Typ.Constant { identifier; _ } ->
        Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
      | Typ.Application { applicand; arguments; _ } ->
        Format.fprintf ppf "@[<2>TypeApplication(@,%a(%a))@]" pp_typ
          applicand
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ") pp_term)
          arguments
      | Typ.ForwardArrow { domain; range; _ } ->
        Format.fprintf ppf "@[<2>TypeArrow(@,%a@ ->@ %a)@]" pp_typ domain
          pp_typ range
      | Typ.BackwardArrow { range; domain; _ } ->
        Format.fprintf ppf "@[<2>TypeArrow(@,%a@ <-@ %a)@]" pp_typ range
          pp_typ domain
      | Typ.Pi
          { parameter_identifier = Option.None; parameter_type; body; _ } ->
        Format.fprintf ppf "@[<2>TypePi(@,{@ _@ :@ %a@ }@ %a)@]" pp_typ
          parameter_type pp_typ body
      | Typ.Pi
          { parameter_identifier = Option.Some parameter_identifier
          ; parameter_type
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TypePi(@,{@ %a@ :@ %a@ }@ %a)@]"
          Identifier.pp parameter_identifier pp_typ parameter_type pp_typ
          body
      | Typ.Parenthesized { typ; _ } ->
        Format.fprintf ppf "@[<2>TypeParenthesized(@,%a)@]" pp_typ typ

    and pp_term ppf term =
      match term with
      | Term.Variable { identifier; _ } ->
        Format.fprintf ppf "%a" Identifier.pp identifier
      | Term.Constant { identifier; _ } ->
        Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
      | Term.Application { applicand; arguments; _ } ->
        Format.fprintf ppf "@[<2>TermApplication(@,%a(%a))@]" pp_term
          applicand
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ") pp_term)
          arguments
      | Term.Abstraction
          { parameter_identifier = Option.None
          ; parameter_type = Option.None
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TermAbstraction(@,\\_.@ %a)@]" pp_term body
      | Term.Abstraction
          { parameter_identifier = Option.None
          ; parameter_type = Option.Some parameter_type
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TermAbstraction(@,\\_:%a.@ %a)@]" pp_typ
          parameter_type pp_term body
      | Term.Abstraction
          { parameter_identifier = Option.Some parameter_identifier
          ; parameter_type = Option.None
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TermAbstraction(@,\\%a.@ %a)@]"
          Identifier.pp parameter_identifier pp_term body
      | Term.Abstraction
          { parameter_identifier = Option.Some parameter_identifier
          ; parameter_type = Option.Some parameter_type
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TermAbstraction(@,\\%a:%a.@ %a)@]"
          Identifier.pp parameter_identifier pp_typ parameter_type pp_term
          body
      | Term.Wildcard _ -> Format.fprintf ppf "_"
      | Term.TypeAnnotated { term; typ; _ } ->
        Format.fprintf ppf "@[<2>TermAnnotated(@,%a@ :@ %a)@]" pp_term term
          pp_typ typ
      | Term.Parenthesized { term; _ } ->
        Format.fprintf ppf "@[<2>TermParenthesized(@,%a)@]" pp_term term
  end
end

module CLF = struct
  open CLF

  let rec pp_typ ppf typ =
    match typ with
    | Typ.Constant { identifier; _ } ->
      Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
    | Typ.Application { applicand; arguments = []; _ } ->
      Format.fprintf ppf "@[<2>%a@]" pp_typ applicand
    | Typ.Application
        { applicand = Typ.Constant { identifier; operator; _ }
        ; arguments
        ; _
        } -> (
      match Operator.fixity operator with
      | Fixity.Prefix ->
        Format.fprintf ppf "@[<2>%a@ %a@]" QualifiedIdentifier.pp identifier
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_term)
          arguments
      | Fixity.Infix ->
        assert (List.length arguments = 2);
        let[@warning "-8"] [ left_argument; right_argument ] = arguments in
        Format.fprintf ppf "@[<2>%a@ %a@ %a@]" pp_term left_argument
          QualifiedIdentifier.pp identifier pp_term right_argument
      | Fixity.Postfix ->
        assert (List.length arguments = 1);
        let[@warning "-8"] [ argument ] = arguments in
        Format.fprintf ppf "@[<2>%a@ %a@]" pp_term argument
          QualifiedIdentifier.pp identifier)
    | Typ.Application { applicand; arguments; _ } ->
      Format.fprintf ppf "@[<2>%a@ %a@]" pp_typ applicand
        (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_term)
        arguments
    | Typ.ForwardArrow { domain; range; _ } ->
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]" pp_typ domain pp_typ range
    | Typ.BackwardArrow { range; domain; _ } ->
      Format.fprintf ppf "@[<2>%a@ <-@ %a@]" pp_typ range pp_typ domain
    | Typ.Pi { parameter_identifier = Option.None; parameter_type; body; _ }
      ->
      Format.fprintf ppf "@[<2>{@ _@ :@ %a@ }@ %a@]" pp_typ parameter_type
        pp_typ body
    | Typ.Pi
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_type
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>{@ %a@ :@ %a@ }@ %a@]" Identifier.pp
        parameter_identifier pp_typ parameter_type pp_typ body
    | Typ.Block { elements = (Option.None, typ), []; _ } ->
      Format.fprintf ppf "@[<2>block %a]" pp_typ typ
    | Typ.Block { elements = (Option.None, _typ), _nt1 :: _nts; _ } ->
      raise
      @@ Invalid_argument
           "[pp_typ] missing identifier for first type in block"
    | Typ.Block { elements = (Option.Some i1, t1), nts; _ } ->
      Format.fprintf ppf "@[<2>block (%a)]"
        (List.pp
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,")
           (fun ppf (i, t) ->
             Format.fprintf ppf "%a@ :@ %a" Identifier.pp i pp_typ t))
        ((i1, t1) :: nts)
    | Typ.Parenthesized { typ; _ } ->
      Format.fprintf ppf "@[<2>(%a)@]" pp_typ typ

  and pp_term ppf term =
    match term with
    | Term.Variable { identifier; _ } ->
      Format.fprintf ppf "%a" Identifier.pp identifier
    | Term.Constant { identifier; _ } ->
      Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
    | Term.Application { applicand; arguments = []; _ } ->
      Format.fprintf ppf "@[<2>%a@]" pp_term applicand
    | Term.Application
        { applicand = Term.Constant { identifier; operator; _ }
        ; arguments
        ; _
        } -> (
      match Operator.fixity operator with
      | Fixity.Prefix ->
        Format.fprintf ppf "@[<2>%a@ %a@]" QualifiedIdentifier.pp identifier
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_term)
          arguments
      | Fixity.Infix ->
        assert (List.length arguments = 2);
        let[@warning "-8"] [ left_argument; right_argument ] = arguments in
        Format.fprintf ppf "@[<2>%a@ %a@ %a@]" pp_term left_argument
          QualifiedIdentifier.pp identifier pp_term right_argument
      | Fixity.Postfix ->
        assert (List.length arguments = 1);
        let[@warning "-8"] [ argument ] = arguments in
        Format.fprintf ppf "@[<2>%a@ %a@]" pp_term argument
          QualifiedIdentifier.pp identifier)
    | Term.Application { applicand; arguments; _ } ->
      Format.fprintf ppf "@[<2>%a@ %a@]" pp_term applicand
        (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_term)
        arguments
    | Term.Abstraction
        { parameter_identifier = Option.None
        ; parameter_type = Option.None
        ; body
        ; _
        } -> Format.fprintf ppf "@[<2>\\_.@ %a@]" pp_term body
    | Term.Abstraction
        { parameter_identifier = Option.None
        ; parameter_type = Option.Some parameter_type
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>\\_:%a.@ %a@]" pp_typ parameter_type pp_term
        body
    | Term.Abstraction
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_type = Option.None
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>\\%a.@ %a@]" Identifier.pp
        parameter_identifier pp_term body
    | Term.Abstraction
        { parameter_identifier = Option.Some parameter_identifier
        ; parameter_type = Option.Some parameter_type
        ; body
        ; _
        } ->
      Format.fprintf ppf "@[<2>\\%a:%a.@ %a@]" Identifier.pp
        parameter_identifier pp_typ parameter_type pp_term body
    | Term.Hole _ -> Format.fprintf ppf "_"
    | Term.Substitution { term; substitution; _ } ->
      Format.fprintf ppf "@[<2>%a%a@]" pp_term term pp_substitution
        substitution
    | Term.Tuple { terms; _ } ->
      Format.fprintf ppf "@[<2><%a>@]"
        (List1.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@,") pp_term)
        terms
    | Term.Projection { term; projection = `By_position i; _ } ->
      Format.fprintf ppf "@[<2>%a.%d@]" pp_term term i
    | Term.Projection { term; projection = `By_identifier i; _ } ->
      Format.fprintf ppf "@[<2>%a.%a@]" pp_term term Identifier.pp i
    | Term.TypeAnnotated { term; typ; _ } ->
      Format.fprintf ppf "@[<2>%a@ :@ %a@]" pp_term term pp_typ typ
    | Term.Parenthesized { term; _ } ->
      Format.fprintf ppf "@[<2>(%a)@]" pp_term term

  and pp_substitution ppf substitution =
    match substitution with
    | Substitution.{ extends_identity = false; terms = []; _ } ->
      Format.fprintf ppf "[]"
    | Substitution.{ extends_identity = true; terms = []; _ } ->
      Format.fprintf ppf "[..]"
    | Substitution.{ extends_identity = false; terms; _ } ->
      Format.fprintf ppf "@[<2>[%a]@]"
        (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,") pp_term)
        terms
    | Substitution.{ extends_identity = true; terms; _ } ->
      Format.fprintf ppf "@[<2>[..,@,%a]@]"
        (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,") pp_term)
        terms

  module Debug = struct
    let rec pp_typ ppf typ =
      match typ with
      | Typ.Constant { identifier; _ } ->
        Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
      | Typ.Application { applicand; arguments; _ } ->
        Format.fprintf ppf "@[<2>TypeApplication(%a(%a))@]" pp_typ applicand
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ") pp_term)
          arguments
      | Typ.ForwardArrow { domain; range; _ } ->
        Format.fprintf ppf "@[<2>TypeArrow(%a@ ->@ %a)@]" pp_typ domain
          pp_typ range
      | Typ.BackwardArrow { range; domain; _ } ->
        Format.fprintf ppf "@[<2>TypeArrow(%a@ <-@ %a)@]" pp_typ range pp_typ
          domain
      | Typ.Pi
          { parameter_identifier = Option.None; parameter_type; body; _ } ->
        Format.fprintf ppf "@[<2>TypePi({@ _@ :@ %a@ }@ %a)@]" pp_typ
          parameter_type pp_typ body
      | Typ.Pi
          { parameter_identifier = Option.Some parameter_identifier
          ; parameter_type
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TypePi({@ %a@ :@ %a@ }@ %a)@]" Identifier.pp
          parameter_identifier pp_typ parameter_type pp_typ body
      | Typ.Block { elements = (Option.None, typ), []; _ } ->
        Format.fprintf ppf "@[<2>TypeBlock(block %a)]" pp_typ typ
      | Typ.Block { elements = (Option.None, _typ), _nt1 :: _nts; _ } ->
        raise
        @@ Invalid_argument
             "[pp_typ] missing identifier for first type in block"
      | Typ.Block { elements = (Option.Some i1, t1), nts; _ } ->
        Format.fprintf ppf "@[<2>TypeBlock(block (%a))]"
          (List.pp
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,")
             (fun ppf (i, t) ->
               Format.fprintf ppf "%a@ :@ %a" Identifier.pp i pp_typ t))
          ((i1, t1) :: nts)
      | Typ.Parenthesized { typ; _ } ->
        Format.fprintf ppf "@[<2>TypeParenthesized(%a)@]" pp_typ typ

    and pp_term ppf term =
      match term with
      | Term.Variable { identifier; _ } ->
        Format.fprintf ppf "%a" Identifier.pp identifier
      | Term.Constant { identifier; _ } ->
        Format.fprintf ppf "%a" QualifiedIdentifier.pp identifier
      | Term.Application { applicand; arguments; _ } ->
        Format.fprintf ppf "@[<2>TermApplication(%a(%a))@]" pp_term applicand
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ") pp_term)
          arguments
      | Term.Abstraction
          { parameter_identifier = Option.None
          ; parameter_type = Option.None
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TermAbstraction(\\_.@ %a)@]" pp_term body
      | Term.Abstraction
          { parameter_identifier = Option.None
          ; parameter_type = Option.Some parameter_type
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TermAbstraction(\\_:%a.@ %a)@]" pp_typ
          parameter_type pp_term body
      | Term.Abstraction
          { parameter_identifier = Option.Some parameter_identifier
          ; parameter_type = Option.None
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TermAbstraction(\\%a.@ %a)@]" Identifier.pp
          parameter_identifier pp_term body
      | Term.Abstraction
          { parameter_identifier = Option.Some parameter_identifier
          ; parameter_type = Option.Some parameter_type
          ; body
          ; _
          } ->
        Format.fprintf ppf "@[<2>TermAbstraction(\\%a:%a.@ %a)@]"
          Identifier.pp parameter_identifier pp_typ parameter_type pp_term
          body
      | Term.Hole { variant = `Underscore; _ } -> Format.fprintf ppf "_"
      | Term.Hole { variant = `Unlabelled; _ } -> Format.fprintf ppf "?"
      | Term.Hole { variant = `Labelled label; _ } ->
        Format.fprintf ppf "?%s" label
      | Term.Substitution { term; substitution; _ } ->
        Format.fprintf ppf "@[<2>TermSubstitution(%a%a)@]" pp_term term
          pp_substitution substitution
      | Term.Tuple { terms; _ } ->
        Format.fprintf ppf "@[<2>TermTuple(<%a>)@]"
          (List1.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@,") pp_term)
          terms
      | Term.Projection { term; projection = `By_position i; _ } ->
        Format.fprintf ppf "@[<2>TermProjection(%a.%d)@]" pp_term term i
      | Term.Projection { term; projection = `By_identifier i; _ } ->
        Format.fprintf ppf "@[<2>TermProjection(%a.%a)@]" pp_term term
          Identifier.pp i
      | Term.TypeAnnotated { term; typ; _ } ->
        Format.fprintf ppf "@[<2>TermAnnotated(%a@ :@ %a)@]" pp_term term
          pp_typ typ
      | Term.Parenthesized { term; _ } ->
        Format.fprintf ppf "@[<2>TermParenthesized(%a)@]" pp_term term

    and pp_substitution ppf substitution =
      match substitution with
      | Substitution.{ extends_identity = false; terms = []; _ } ->
        Format.fprintf ppf "Substitution([])"
      | Substitution.{ extends_identity = true; terms = []; _ } ->
        Format.fprintf ppf "Substitution([..])"
      | Substitution.{ extends_identity = false; terms; _ } ->
        Format.fprintf ppf "@[<2>Substitution([%a])@]"
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,") pp_term)
          terms
      | Substitution.{ extends_identity = true; terms; _ } ->
        Format.fprintf ppf "@[<2>Substitution([..,@,%a])@]"
          (List.pp ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,") pp_term)
          terms
  end
end
