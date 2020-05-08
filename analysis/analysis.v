Require Import Coq.Strings.Ascii.

Inductive regex : Type :=
| empty
| epsilon
| char (c:ascii)
| seq (re1 re2 : regex)
| alt (re1 re2 : regex)
| star (re' : regex).

Fixpoint s (re : regex) : nat :=
  match re with
  | empty       => 0
  | epsilon     => 1
  | char _      => 1
  | seq re1 re2 => 1 + s re1 + s re2
  | alt re1 re2 => 1 + s re1 + s re2
  | star re'    => 1 + s re'
  end.

Fixpoint h (re : regex) : nat :=
  match re with
  | empty       => 0
  | epsilon     => 1
  | char _      => 1
  | seq re1 re2 => 1 + max (h re1) (h re2)
  | alt re1 re2 => 1 + max (h re1) (h re2)
  | star re'    => 1 + h re'
  end.

Fixpoint n (re : regex) : bool :=
  match re with
  | empty       => false
  | epsilon     => true
  | char _      => false
  | seq re1 re2 => n re1 && n re2
  | alt re1 re2 => n re1 || n re2
  | star re'    => true
  end.

Fixpoint d (c : ascii) (re : regex) : regex :=
  match re with
  | empty       => empty
  | epsilon     => empty
  | char c'     => if eqb c c'
                   then epsilon
                   else empty
  | seq re1 re2 => if n re1
                   then alt (seq (d c re1) re2) (d c re2)
                   else      seq (d c re1) re2
  | alt re1 re2 => alt (d c re1) (d c re2)
  | star re'    => seq (d c re') re
  end.

Require Import Coq.micromega.Lia.

Lemma deriv_height : forall c re,
    h (d c re) <= 2 * h re.
Proof.
  induction re; try solve [ simpl; lia ].
  - simpl. destruct (eqb_spec c c0); simpl; lia.
  - unfold d. fold d. destruct (n re1).
    + unfold h. fold h.
      lia.
    + unfold h. fold h. lia.
Qed.

Lemma deriv_height_k : forall c re,
    h (d c re) <= 4 + h re.
Proof.
  induction re; try solve [ simpl; lia ].
  - simpl. destruct (eqb_spec c c0); simpl; lia.
  - unfold d. fold d. destruct (n re1).
    + unfold h. fold h.
      repeat rewrite <- PeanoNat.Nat.add_max_distr_l.
      apply Max.max_lub.
      * repeat rewrite -> PeanoNat.Nat.add_max_distr_l. simpl.
        repeat apply le_n_S. admit.
      * lia.
    + unfold h. fold h. lia.
Abort.

Lemma h_seq_re1 : forall c re1 re2,
    1 + h (d c re1) <= h (d c (seq re1 re2)).
Proof.
  intros.
  unfold d. fold d.
  destruct (n re1); unfold h; fold h.
  - apply le_n_S.
    destruct (PeanoNat.Nat.max_dec (1 + Nat.max (h (d c re1)) (h re2)) (h (d c re2))); lia.
  - destruct (PeanoNat.Nat.max_dec (h (d c re1)) (h re2)); lia.
Qed.

Lemma h_seq_re1_p2 : forall c re2, exists re1,
      2 + h (d c re1) <= h (d c (seq re1 re2)).
Proof.
  intros c re2.
  exists (alt epsilon (char c)).
  unfold d. fold d.
  replace (n (alt epsilon (char c))) with true by reflexivity.
  unfold h. fold h.
  rewrite eqb_refl.
  rewrite Max.max_0_l.
  replace (h epsilon) with 1 by reflexivity.
  lia.
Qed.

Lemma h_seq_nre1_p2 : forall c re1 re2,
    n re1 = true ->
    h re2 <= h (d c re1) ->
    h (d c re2) <= h (d c re1) ->
    2 + h (d c re1) = h (d c (seq re1 re2)).
Proof.
  intros.
  unfold d. fold d. rewrite H.
  unfold h. fold h.
  lia.
Qed.

Lemma exact_height : forall c re k,
    0 < k ->
    0 < h re ->
    h (d c re) = k + h re ->
    h (d c (alt epsilon re)) = k + h (alt epsilon re).
Proof.
  intros. unfold d. fold d. unfold h. fold h. rewrite Max.max_0_l.
  lia.
Qed.

Lemma deriv_height_constant :
  ~ exists k,
      (forall c re, h (d c re) <= k + h re) /\
      (forall c, exists re, n re = true /\ h (d c re) = k + h re).
Proof.
  intros contra; destruct contra as [k [H Hexact]].
  specialize (Hexact zero).
  destruct Hexact as [re1 [Hn Hexact]].
  assert (2 + h (d zero re1) = h (d zero (seq re1 empty))) as H2.
  { apply h_seq_nre1_p2; auto; try solve [ simpl; lia ]. }
  assert (h (d zero (seq re1 empty)) <= k + h (seq re1 empty)) as Hseq by apply H.
  assert (h (seq re1 empty) = 1 + h re1) as Hh by solve [ simpl; lia ].
  replace (h (seq re1 empty)) with (1 + h re1) in Hseq.
  lia.
Qed.

Lemma deriv_height_constant_general :
  ~ exists k,
      (forall c re, h (d c re) <= k + h re) /\
      (forall c, exists re, h (d c re) = k + h re).
Proof.
  intros contra; destruct contra as [k [H Hexact]].

  (* k is DEFINITELY not 0 *)
  assert (0 < k) as Hk.
  { destruct (PeanoNat.Nat.lt_decidable 0 k); auto.
    assert (k = 0) as Hk by lia.
    specialize (H zero (star (char zero))). simpl in H.
    subst. inversion H. inversion H2. inversion H4.
  }

  (* finding a good LHS of a sequence *)
  specialize (Hexact zero).
  destruct Hexact as [re1 Hexact].
  pose (re1' := (alt epsilon re1)).
  assert (n re1' = true) as Hn by auto.
  assert (0 < h re1) as Hhre1.
  { destruct re1; simpl; inversion Hexact; lia. }
  assert (0 < h re1') as Hhre1' by solve [ simpl; lia ].
  assert (h (d zero re1') = k + h re1') as Hexact'.
  { subst re1'. unfold d. fold d. unfold h. fold h. lia. }

  (* uh oh *)
  assert (2 + h (d zero re1') = h (d zero (seq re1' empty))) as H2.
  { apply h_seq_nre1_p2; auto; try solve [ simpl; lia ]. }
  assert (h (d zero (seq re1' empty)) <= k + h (seq re1' empty)) as Hseq by apply H.
  assert (h (seq re1' empty) = 1 + h re1') as Hh by solve [ simpl; lia ].
  replace (h (seq re1' empty)) with (1 + h re1') in Hseq.
  rewrite <- H2 in Hseq.
  lia.
Qed.