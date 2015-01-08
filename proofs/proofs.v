
Set Implicit Arguments.



Inductive Time : Set :=
 | Z : Time
 | S : Time -> Time.

Inductive leqtime : Time -> Time -> Prop :=
| leq_same : forall (l : Time), leqtime l l
| leq_S    : forall (l r : Time), leqtime l r -> leqtime l (S r).


Lemma leq_trans : forall (a b c : Time), leqtime a b -> leqtime b c -> leqtime a c.
intros.
induction H0.
assumption.
specialize (IHleqtime H).
apply leq_S.
assumption.
Qed.

CoInductive Ev (A : Set) : Set :=
 | CZ : A -> Ev A
 | CS : Ev A -> Ev A.


Fixpoint obs_ev {A : Set} (e : Ev A) (t : Time) : option A := 
  match e, t with
  | CZ x , _   => Some x
  | _    , Z   => None
  | CS e , S t => obs_ev e t
  end.

Fixpoint leqT {A : Set } (e : Ev A) (t : Time) : bool :=
 match e, t with
  | CZ x , _   => true
  | _    , Z   => false
  | CS e , S t => leqT e t
  end.

Fixpoint inject {A : Set} (t : Time) (a : A) : Ev A :=
match t with
 | Z => CZ a
 | S x => CS (inject x a)
end.



CoFixpoint never {A :Set} : Ev A :=
  CS never.

CoFixpoint max {A : Set} (a : Ev A) (b : Ev A) :=
  match a, b with
  |CZ _, CZ a => CZ a
  | CS a, CZ _ => CS a
  | CZ _, CS b => CS b
  | CS a, CS b => CS (max a b)
  end.

CoFixpoint min {A : Set} (a : Ev A) (b : Ev A) :=
  match a, b with
  |_, CZ a => CZ a
  | CZ a, _ => CZ a
  | CS a, CS b => CS (min a b)
  end.

Definition unfoldev A (s : Ev A) : Ev A :=
  match s with
    | CZ x => CZ x
    | CS e => CS e
  end.

CoInductive ev_eq {A : Set} : Ev A -> Ev A -> Prop :=
  | ev_refl : forall x : Ev A, ev_eq x x
  | ev_bi   : forall x y: Ev A, ev_eq x y -> ev_eq (CS x) (CS y).

Theorem unfold_eq : forall A (s : Ev A), s = unfoldev s.
destruct s.
simpl.
reflexivity.
simpl.
reflexivity.
Qed.


Require Import Coq.Setoids.Setoid.

Lemma dest_ev : forall {A : Set} (a : Ev A), (exists x, a = CZ x) \/ (exists p : Ev A, a = CS p).
intros.
destruct a.
left.
exists a.
reflexivity.
right.
exists a.
reflexivity.
Qed.

Lemma unitr_min : forall {A : Set} (a : Ev A), ev_eq (min a never) a.
cofix.
intros.
rewrite (unfold_eq never).
simpl.
elim (dest_ev a).
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (min (CZ x) (CS never))).
simpl.
apply ev_refl.
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (min (CS x) (CS never))).
simpl.
apply ev_bi.
exact (unitr_min A x).
Qed.

Lemma unitl_min : forall {A : Set} (a : Ev A), ev_eq (min never a) a.
cofix.
intros.
rewrite (unfold_eq never).
simpl.
elim (dest_ev a).
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (min (CS never) (CZ x))).
simpl.
apply ev_refl.
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (min (CS never) (CS x))).
simpl.
apply ev_bi.
exact (unitl_min A x).
Qed.


Lemma zeror_max : forall {A : Set} (a : Ev A), ev_eq (max a never) (@never A).
cofix.
intros.
rewrite (unfold_eq never).
simpl.
elim (dest_ev a).
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (max (CZ x) (CS never))).
simpl.
apply ev_refl.
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (max (CS x) (CS never))).
simpl.
apply ev_bi.
exact (zeror_max A x).
Qed.

Lemma zerol_max : forall {A : Set} (a : Ev A), ev_eq (max never a) (@never A).
cofix.
intros.
rewrite (unfold_eq never).
simpl.
elim (dest_ev a).
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (max (CS never) (CZ x))).
simpl.
apply ev_refl.
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (max (CS never) (CS x))).
simpl.
apply ev_bi.
exact (zerol_max A x).
Qed.

CoFixpoint bindEv {A B : Set} (a : Ev A) (f : A -> Ev B) : Ev B :=
  match a with
  |CZ a => f a
  |CS p => CS (bindEv p f)
  end.

Lemma zero_bind : forall {A B : Set} (f : A -> Ev B), ev_eq (bindEv never f) (@never B).
intros.
cofix.
rewrite (unfold_eq (@never A)).
rewrite (unfold_eq (@never B)).
simpl.
rewrite (unfold_eq (bindEv (CS never) f)).
simpl.
apply ev_bi.
assumption.
Qed.

Definition returnEv {A: Set} (a : A) : Ev A := CZ a.

(* Prove that Ev is a monad *)
Lemma bind_one_r : forall {A : Set} (m : Ev A), ev_eq m (bindEv m returnEv).
cofix.
intros.
unfold returnEv.
elim (dest_ev m).
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (bindEv (CZ x) (fun a : A => CZ a))).
simpl.
apply ev_refl.
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (bindEv (CS x) (fun a : A => CZ a))).
simpl.
apply ev_bi.
exact (bind_one_r A x).
Qed.

Lemma bind_one_l : forall {A B : Set} (a : A) (k : A -> Ev B), ev_eq (k a) (bindEv (returnEv a) k).
intros.
unfold returnEv.
rewrite (unfold_eq (bindEv (CZ a) k)).
simpl.
elim (dest_ev (k a)).
intros.
elim H.
intros.
rewrite H0.
apply ev_refl.
intros.
elim H.
intros.
rewrite H0.
apply ev_refl.
Qed.



Lemma bind_assoc : forall {A B C: Set} (m : Ev A) (k : A -> Ev B) (h : B -> Ev C),
  ev_eq (bindEv m (fun x => bindEv (k x) h)) (bindEv (bindEv m k) h).
intros A B C.
cofix.
intros.
elim (dest_ev m).
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (bindEv (CZ x) (fun x0 : A => bindEv (k x0) h))).
rewrite (unfold_eq (bindEv (bindEv (CZ x) k) h)).
simpl.
apply ev_refl.
intros.
elim H.
intros.
rewrite H0.
rewrite (unfold_eq (bindEv (CS x) (fun x0 : A => bindEv (k x0) h))).
rewrite (unfold_eq (bindEv (bindEv (CS x) k) h)).
simpl.
apply ev_bi.
exact (bind_assoc x k h).
Qed.

Inductive Beh (A : Set) : Set :=
 | Bh : (Time -> A) -> Beh A.

Definition getFun {A : Set} (a : Beh A) : Time -> A := match a with
 | Bh x => x
 end.

Definition returnB {A : Set} (a : A) : Beh A := Bh (fun x => a).
Definition bindB  {A B :Set} (m : Beh A) (f : A -> Beh B) : Beh B := Bh (fun t => getFun (f (getFun m t)) t ).



Require Import Coq.Logic.FunctionalExtensionality.

Lemma wrap_unwrap_B : forall {A : Set} (b : Beh A), Bh (getFun b) = b.
intros.
destruct b.
unfold getFun.
reflexivity.
Qed.

Lemma unwrap_wrap_B : forall {A : Set} (b : Time -> A), getFun (Bh b) = b.
intros.
unfold getFun.
reflexivity.
Qed.

(* Prove that Beh is a monad *)

Lemma bind_one_r_B : forall {A : Set} (m : Beh A), m = bindB m returnB.
intros.
unfold returnB.
unfold bindB.
simpl.
rewrite <- eta_expansion.
rewrite wrap_unwrap_B.
reflexivity.
Qed.

Lemma bind_one_l_B : forall {A B : Set} (a : A) (k : A -> Beh B), k a = bindB (returnB a) k.
intros.
unfold returnB.
unfold bindB.
simpl.
rewrite <- eta_expansion.
rewrite wrap_unwrap_B.
reflexivity.
Qed.

Lemma bind_assoc_B : forall {A B C: Set} (m : Beh A) (k : A -> Beh B) (h : B -> Beh C),
        bindB m (fun x => bindB (k x) h) = bindB (bindB m k) h.
intros.
unfold bindB.
simpl.
reflexivity.
Qed.

Definition switch {A : Set} (b : Beh A ) (e : Ev (Beh A)) := Bh (fun t =>
                  match obs_ev e t with
                    | Some (Bh a) => a t
                    | None  => getFun b t
                  end).

CoFixpoint getNext {A : Set} (b : Time -> (option A)) (tnow : Time) (tleft : Time): Ev A :=
  match tleft with
  |Z => match b tnow with
            | Some a => CZ a
            | None => CS (getNext b (S tnow) Z)
        end
  |S x => CS (getNext b (S tnow) x)
  end.

Definition whenJust {A : Set} (b : Beh (option A)) := Bh (fun t => getNext (getFun b) Z t).

Inductive EventSyntax : Set -> Type :=
| ERet : forall {x : Set}, x -> EventSyntax x
| EBind : forall {x y: Set}, EventSyntax x -> (x -> EventSyntax y) -> EventSyntax y.

Inductive BehaviorSyntax {event : Type -> Type} {behavior : Type -> Type} : Type -> Type :=
| BRet : forall {x : Set}, x -> BehaviorSyntax x

| BBind : forall {x y: Set}, BehaviorSyntax y ->
          (y -> BehaviorSyntax x) -> BehaviorSyntax x
| Switch : forall {x : Set}, BehaviorSyntax x -> event (behavior x) -> BehaviorSyntax x
| WhenJust : forall {x : Set}, BehaviorSyntax (option x) -> BehaviorSyntax (event x).
(*
Fixpoint toDenotation {A : Set} (s : @FRPSyntax Ev Beh A) : A :=
 match s with
  | EPrim _ t x => inject t x
  | ERet B x => (returnEv x) : Ev B 
  | EBind _ _ m f => bindEv m f
  | BRet _ x => returnB x
  | BBind _ _ m f => bindB m f
  | Switch _ m e => switch m e
  | WhenJust _ b => whenJust b
 end.
*)


Inductive obs_eq : forall (A : Set), Time -> A -> A -> Prop :=
  | eq_refl : forall {A : Set} (t : Time) (a : A), obs_eq t a a
  | eq_b    : forall {A : Set} (t : Time) (a b : Beh A), 
              (forall t', leqtime t t' -> obs_eq t (getFun a t') (getFun b t'))
              -> obs_eq t a b
  | eq_e    : forall {A : Set} (t : Time) (a b : Ev A),
              obs_eq t (obs_ev a t) (obs_ev b t) -> 
              obs_eq t a b.



Lemma obs_eq_sym : forall {A : Set} (a b : A) (t : Time), obs_eq t a b -> obs_eq t b a.
intros.
auto.
induction H.
apply eq_refl.
apply eq_b.
assumption.
apply eq_e.
assumption.
Qed.

Require Import Coq.Program.Equality.

Check JMeq_ind_r.


Check JMeq_eq.

Lemma type_thing : forall {F : Set -> Set } {x y : Set}, F x = F y -> x = y.
intros.
f_equal H.

Lemma obs_eqb_trans : forall {A : Set} (a b c : A) (t : Time), obs_eq t a b -> obs_eq t b c -> obs_eq t a c.
intros.
induction H.
assumption.
apply eq_b.
intros.
specialize (H t' H2).
specialize (H1 t' H2).
specialize (H1 (getFun c t')).
apply H1.
dependent destruction H0.

apply eq_refl.
apply JMeq_eq in x.
simpl_one_dep_JMeq x1.
on_JMeq in x1.
rewrite <- x1 in H0.
apply (H0 t').
apply eq_b in H0.
destruct b.
destruct c.

dependent destruction H0.
apply eq_refl.


dependent destruction H0.
assumption.
destruct b.
rewrite unwrap_wrap_B in H1.


unfold getFun in H1.
simpl in H1.
dependent destruction H0.
assumption.

apply eq_b.
intros.
specialize (H t' H2).
specialize (H1 t' H2).
specialize (H1 (c t')).
apply eq_b in H0.



apply H1.

specialize (@H1 (Beh A) c H0).
intros.

induction H0.
assumption.







Lemma obs_eq_weak : forall {A : Set} (a b : A) (t f : Time), obs_eq t a b -> leqtime t f -> obs_eq f a b.
intros.
elim H.
intros.
apply eq_refl.
induction H0.
apply eq_refl.
apply eq_b.
intros.
exact (H0 t' H3).
intros.
apply eq_refl.
induction H1.
assumption.
induction

Lemma obs_inj_ev : forall {A : Set} (s : A) (l : Time), obs_ev (inject l s) l = Some s.
intros.
induction l.
unfold obs_ev.
unfold inject.
reflexivity.
simpl.
assumption.
Qed.

Lemma obs_eq_inj : forall {A : Set} ( s : A) (t p: Time), leqtime p t -> obs_eq t (inject p s) (inject t s).
intros.
apply eq_e.
induction H.
apply eq_refl.

rewrite obs_inj_ev.
intros.

induction H.
apply eq_refl.
apply eq_e.
intros.
simpl.

simpl.

Lemma forgetPast {A : Set} : forall (s x: Beh A)
           (t : Time), obs_eq t s (switch x (inject t s)).
intros.
apply eq_b.
intros.
induction H.

unfold switch.
rewrite obs_inj_ev.

apply eq_refl.
unfold switch.
simpl.
apply eq_refl.
unfold inject.
unfold switch.
simpl.
simpl.
simpl.
destruct t'.
unfold switch.
simpl.
apply eq_refl.

unfold switch.
simpl.


unfold switch.
unfold obs_ev.
simpl.






