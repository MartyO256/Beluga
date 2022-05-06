open Support

(** Abstract type for ranges determined by start and end delimiters. *)
module type RANGE = sig
  (** The range type on elements of type {!e}*)
  type t

  (** The type of elements in the range of type {!t}. *)
  type e

  (** {1 Constructors} *)

  (** [make ~start_point ~end_point] is the range from [start_point] to
      [end_point]. These delimiters are inclusive.

      @raise Assert_failure if [start_point > end_point].*)
  val make : start_point:e -> end_point:e -> t

  (** [make_from_point p] makes a point range from [p] to [p]. *)
  val make_from_point : e -> t

  (** [join r1 r2] computes the least range that includes both [r1] and [r2]. *)
  val join : t -> t -> t

  (** {1 Destructors} *)

  (** [start_point r] is the start delimiter of range [r]. *)
  val start_point : t -> e

  (** [end_point r] is the end delimiter of range [r]. *)
  val end_point : t -> e

  (** {1 Predicates} *)

  (** [contains range element] is [true] if and only if [element] lies
      inclusively between the delimiters of [range]. *)
  val contains : t -> e -> bool

  (** [includes outer inner] is [true] if and only if the delimiters of
      [inner] are contained in [outer]. *)
  val includes : t -> t -> bool

  (** {1 Instances} *)

  include Eq.EQ with type t := t
end

(** Functor for making range types over totally ordered elements. *)
module Make (Element : Ord.ORD) : RANGE with type e = Element.t
