Require Import Reals.
Require Import Psatz.
Require Import QuantumLib.Complex.
Require Import QuantumLib.Permutations.
Require Import QuantumLib.VectorStates.
Require Import Lists.ListSet.
Require Import MuQSyntax.

Local Open Scope nat_scope.

(* This document contains the type system of MuQ, based on the simple typed lambda calculus type system.
   The typing relation takes in the equivalence relation to switch different kinds of matrices.
   There are three kinds of mtrices: P, H, and U. P refers to a single creator/annihilator operation,
   H refers to a Hermitian matrix where its transpose is the same as the matrix.
   U is the unitary, taking the exponent of the Hermitian matrix. *)

(*
Fixpoint check_base_aux (n:nat) (s:spinbase) (i:nat) (m:nat) :=
   match n with 0 => True
              | S j => check_base_aux j s i m /\ (s i j) < m
   end.

Fixpoint check_base (s:spinbase) (nl:list partype) :=
   match nl with [] => True
              | ((n,m)::ml) => check_base_aux n s (length ml) m /\ check_base s ml
   end.

Fixpoint good_base' (m:nat) (v:nat -> basisket) (nl: list partype) :=
  match m with 0 => True
            | S j => good_base' j v nl /\ check_base (snd (v j)) nl
  end.

Definition good_base (s:parstate) (nl:list partype) := 
  match s with Zero => True
             | Sup m v => good_base' m v nl
  end.
*)

Inductive merge_ttype : list type -> type -> Prop :=
  | merge_ttype_empty : merge_ttype nil (TType nil)
  | merge_ttype_many: forall s x l, merge_ttype l (TType s) -> merge_ttype (QTy (TType x)::l) (TType (x++s)).

Inductive merge_itype : list type -> type -> Prop :=
  | merge_itype_empty : merge_itype nil (IType nil)
  | merge_itype_many: forall s x l, merge_itype l (IType s) -> merge_itype (QTy (IType x)::l) (IType (x++s)).

Inductive merge_ftype : list type -> type -> Prop :=
  | merge_ftype_empty : forall f, merge_ftype nil (FType f nil)
  | merge_ftype_many: forall f s x l, merge_ftype l (FType f s) -> merge_ftype (QTy (FType f x)::l) (FType f (x++s)).

Inductive merge : list type -> type -> Prop :=
  | merge_st: forall tl t, merge_ttype tl t -> merge tl t
  | merge_dot: forall tl t, merge_itype tl t -> merge tl t
  | merge_fun: forall tl t, merge_ftype tl t -> merge tl t.

Definition good_base (s:parstate) (nl:partype) := 
  forall k, k < fst nl -> (snd s) k < snd nl.

Inductive join : typeflag -> typeflag -> typeflag -> Prop :=
  | join_same : forall tf, join tf tf tf
  | join_p1 : forall tf, join P tf P
  | join_p2 : forall tf, join tf P P.

Inductive typing : (var -> type) -> exp -> type -> Prop :=
 (* | tpar : forall g t e1 e2, equiv e1 e2 -> typing g e2 t -> typing g e1 t *)
  | tvar : forall g x, typing g (Var x) (g x)
  | tval : forall g c, typing g (Val c) CT
  | tzero: forall g tl, typing g (Zero tl) (TType tl)
  | tvec : forall g s t, good_base s t -> typing g (St s t) (TType ([t]))
  | top : forall g j c t tf, j < fst t -> typing g (Anni j c t tf) (FType tf [t])
  | tlambda: forall g y t ea t', typing (update g y t) ea t' -> typing g (Lambda y t ea) t'
  | tmu : forall g y t ea, typing (update g y (FTy t t)) ea t -> typing g (Mu y t ea) (FTy t t)
  | tdag : forall g e t, typing g e (TType t) -> typing g (Trans e) (IType t)
  | tdagr : forall g e t, typing g e (IType t) -> typing g (Trans e) (TType t)
  | ttrans: forall g e tf t, typing g e (FType tf t) -> typing g (Trans e) (FType tf t)
  | ttensor: forall g e tl t, Forall2 (fun x y => typing g x y) e tl -> merge tl t -> typing g (Tensor e) t
  | tplus: forall g e t, Forall (fun x => typing g x t) e -> typing g (Plus e) t
  | tapp: forall g e1 e2 t1 t2, typing g e1 (FTy t1 t2) -> typing g e2 t1 -> typing g (App e1 e2) t2
  | tmat: forall g e1 e2 tf t, typing g e1 (FType tf t) -> typing g e2 (TType t) -> typing g (App e1 e2) (TType t)
  | tinner: forall g e1 e2 t, typing g e1 (IType t) -> typing g e2 (TType t) -> typing g (App e1 e2) CT
  | tseq: forall g e1 e2 tf1 tf2 tf3 t, typing g e1 (FType tf1 t) -> typing g e2 (FType tf2 t) 
                                -> join tf1 tf2 tf3 -> typing g (App e1 e2) (FType tf3 t)
  | tnor1: forall g e t, typing g e (TType t) -> typing g (Nor e) (TType t)
  | tnor2: forall g e t, typing g e (IType t) -> typing g (Nor e) (IType t)
  | ther : forall g e t, typing g e (FType P t) -> equiv (Trans e) e -> typing g e (FType H t)
  | texp : forall g e t, typing g e (FType H t) -> typing g (Exp e) (FType U t)
  | tlog : forall g e t, typing g e (FType U t) -> typing g (Log e) (FType H t).

Lemma tvar_eq: forall e1 e2 x, equiv e1 e2 -> e1 = Var x -> e2 = Var x.
Proof.
  intros.
  induction H; try easy. subst.
  assert (Var x = Var x) by easy. apply IHequiv1 in H0 as X1. subst.
  apply IHequiv2 in H0. subst. easy.
Qed.

Lemma tval_eq: forall e1 e2 c, equiv e1 e2 -> e1 = Val c -> e2 = Val c.
Proof.
  intros.
  induction H; try easy. subst.
  assert (Val c = Val c) by easy. apply IHequiv1 in H0 as X1. subst.
  apply IHequiv2 in H0. subst. easy.
Qed.

Lemma tzero_eq: forall e1 e2 tl, equiv e1 e2 -> e1 = Zero tl -> e2 = Zero tl.
Proof.
  intros.
  induction H; try easy. subst.
  assert (Zero tl = Zero tl) by easy. apply IHequiv1 in H0 as X1. subst.
  apply IHequiv2 in H0. subst. easy.
Qed.

Lemma tst_eq: forall e1 e2 s t, equiv e1 e2 -> e1 = St s t -> e2 = St s t.
Proof.
  intros.
  induction H; try easy. subst.
  assert (St s t = St s t) by easy. apply IHequiv1 in H0 as X1. subst.
  apply IHequiv2 in H0. subst. easy.
Qed.

Lemma tan_eq: forall e1 e2 s c t tf, equiv e1 e2 -> e1 = Anni s c t tf -> e2 = Anni s c t tf.
Proof.
  intros.
  induction H; try easy. subst.
  assert (Anni s c t tf = Anni s c t tf) by easy. apply IHequiv1 in H0 as X1. subst.
  apply IHequiv2 in H0. subst. easy.
Qed.

Lemma type_equiv : forall e g t e1, typing g e t -> equiv e e1 -> typing g e1 t.
Proof.
  intros. generalize dependent e1. induction H; intros; simpl in *; try easy.
  apply tvar_eq with (x := x) in H0; try easy. subst.
  constructor.
  apply tval_eq with (c := c) in H0; try easy. subst.
  constructor.
  eapply tzero_eq in H0; try easy. subst. constructor.
  eapply tst_eq in H0; try easy. subst. constructor. easy.
  eapply tan_eq in H0; try easy. subst. constructor. easy.
Admitted.

(*
Check typing_ind.

Lemma typing_ind'
     : forall P0 : (var -> type) -> exp -> type -> Prop,
       (forall (g : var -> type) (t : type) (e1 e2 : exp),
        equiv e1 e2 -> typing g e2 t -> P0 g e2 t -> P0 g e1 t) ->
       (forall (g : var -> type) (x : var), P0 g (Var x) (g x)) ->
       (forall (g : var -> type) (c : C), P0 g (Val c) CT) ->
       (forall (g : var -> type) (s : parstate) (t : list partype) (t' : qtype),
        good_base s t -> toTensor t = Some t' -> P0 g (St s t) t') ->
       (forall (g : var -> type) (j : nat) (c : C) (t: partype) (tf : typeflag),
        j < fst t -> P0 g (Anni j c t tf) (FType tf (SType t))) ->
       (forall (g : nat -> type) (y : nat) (t : type) (ea : exp) (t' : type),
        typing (update g y t) ea t' -> P0 (update g y t) ea t' -> P0 g (Lambda y t ea) t') ->
       (forall (g : nat -> type) (y : nat) (t : type) (ea : exp),
        typing (update g y (FTy t t)) ea t -> P0 (update g y (FTy t t)) ea t -> P0 g (Mu y t ea) (FTy t t)) ->
       (forall (g : var -> type) (e : exp) (t : qtype), typing g e (TType t) -> P0 g e (TType t) -> P0 g (Trans e) (IType t)) ->
       (forall (g : var -> type) (e : exp) (t : qtype), typing g e (IType t) -> P0 g e (IType t) -> P0 g (Trans e) t) ->
       (forall (g : var -> type) (e : exp) (tf : typeflag)
          (t : qtype),
        typing g e (FType tf t) ->
        P0 g e (FType tf t) -> P0 g (Trans e) (FType tf t)) ->
       (forall (g : var -> type) (e1 e2 : exp) (t1 t2 t3 : type),
        typing g e1 t1 ->
        P0 g e1 t1 ->
        typing g e2 t2 ->
        P0 g e2 t2 -> merge t1 t2 t3 -> P0 g (Tensor e1 e2) t3) ->
       (forall (g : var -> type) (e1 e2 : exp) (t : type),
        typing g e1 t ->
        P0 g e1 t ->
        typing g e2 t -> P0 g e2 t -> P0 g (Plus e1 e2) t) ->
       (forall (g : var -> type) (e1 e2 : exp) (t1 t2 : type),
        typing g e1 (FTy t1 t2) ->
        P0 g e1 (FTy t1 t2) ->
        typing g e2 t1 -> P0 g e2 t1 -> P0 g (App e1 e2) t2) ->
       (forall (g : var -> type) (e1 e2 : exp) 
          (tf : typeflag) (t : qtype),
        typing g e1 (FType tf t) ->
        P0 g e1 (FType tf t) ->
        typing g e2 t -> P0 g e2 t -> P0 g (App e1 e2) t) ->
       (forall (g : var -> type) (e1 e2 : exp) (t : qtype),
        typing g e1 (IType t) ->
        P0 g e1 (IType t) ->
        typing g e2 t -> P0 g e2 t -> P0 g (App e1 e2) CT) ->
       (forall (g : var -> type) (e1 e2 : exp)
          (tf1 tf2 tf3 : typeflag) (t : qtype),
        typing g e1 (FType tf1 t) ->
        P0 g e1 (FType tf1 t) ->
        typing g e2 (FType tf2 t) ->
        P0 g e2 (FType tf2 t) ->
        join tf1 tf2 tf3 -> P0 g (App e1 e2) (FType tf3 t)) ->
       (forall (g : var -> type) (e : exp) (t : qtype),
        typing g e t -> P0 g e t -> P0 g (Nor e) t) ->
       (forall (g : var -> type) (e : exp) (t : qtype),
        typing g e (IType t) ->
        P0 g e (IType t) -> P0 g (Nor e) (IType t)) ->
       (forall (g : var -> type) (e : exp) (t : qtype),
        typing g e (FType P t) ->
        P0 g e (FType P t) ->
        equiv (Trans e) e -> P0 g e (FType H t)) ->
       (forall (g : var -> type) (e : exp) (t : qtype),
        typing g e (FType H t) ->
        P0 g e (FType H t) -> P0 g (Exp e) (FType U t)) ->
       (forall (g : var -> type) (e : exp) (t : qtype),
        typing g e (FType U t) ->
        P0 g e (FType U t) -> P0 g (Log e) (FType H t)) ->
       forall (t : var -> type) (e : exp) (t0 : type),
       typing t e t0 -> P0 t e t0.
Proof.

Admitted.
*)

