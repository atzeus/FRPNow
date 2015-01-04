
Set Implicit Arguments.



Inductive Time : Set :=
 | Z : Time
 | S : Time -> Time.

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

Definition Beh (A : Set) := Time -> A.

Definition returnB {A : Set} (a : A) : Beh A := fun x => a.
Definition bindB  {A B :Set} (m : Beh A) (f : A -> Beh B) := fun t => f (m t) t.

Require Import Coq.Logic.FunctionalExtensionality.

(* Prove that Ev is a monad *)

Lemma bind_one_r_B : forall {A : Set} (m : Beh A), m = bindB m returnB.
intros.
unfold returnB.
unfold bindB.
simpl.
apply functional_extensionality.
intros.
reflexivity.
Qed.

Lemma bind_one_l_B : forall {A B : Set} (a : A) (k : A -> Beh B), k a = bindB (returnB a) k.
intros.
unfold returnB.
unfold bindB.
apply functional_extensionality.
reflexivity.
Qed.

Lemma bind_assoc_B : forall {A B C: Set} (m : Beh A) (k : A -> Beh B) (h : B -> Beh C),
        bindB m (fun x => bindB (k x) h) = bindB (bindB m k) h.
intros.
unfold bindB.
apply functional_extensionality.
reflexivity.
Qed.

Definition switch {A : Set} (b : Beh A ) (e : Ev (Beh A)) := fun t =>
                  match obs_ev e t with
                    | Some a => a t
                    | None  => b t
                  end.

CoFixpoint getNext {A : Set} (b : Beh (option A)) (tnow : Time) (tleft : Time): Ev A :=
  match tleft with
  |Z => match b tnow with
            | Some a => CZ a
            | None => CS (getNext b (S tnow) Z)
        end
  |S x => CS (getNext b (S tnow) x)
  end.

Definition whenJust {A : Set} (b : Beh (option A)) := fun t => getNext b Z t.


Inductive FRPSyntax {event : Set -> Set} {behavior : Set -> Set}: Set -> Set :=
| ERet : forall x : Set , x -> FRPSyntax (event x)
| EBind : forall x y: Set, event x -> (x -> event y) -> FRPSyntax (event y)
| BRet : forall x : Set, x -> FRPSyntax (behavior x)

| BBind : forall x y: Set, behavior x ->
          (x -> behavior y) -> FRPSyntax (behavior y)
| Switch : forall x : Set, behavior x -> event (behavior x) -> FRPSyntax (behavior x)

| WhenJust : forall x : Set, behavior (option x) -> FRPSyntax (behavior (event x)).

Fixpoint toDenotation {A : Set} (s : @FRPSyntax Ev Beh A) : A :=
 match s with
  | ERet B x => (returnEv x) : Ev B 
  | EBind _ _ m f => bindEv m f
  | BRet _ x => returnB x
  | BBind _ _ m f => bindB m f
  | Switch _ m e => switch m e
  | WhenJust _ b => whenJust b
 end.






