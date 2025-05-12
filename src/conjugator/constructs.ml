open Infixes

type conjugated_verb = {
  stem: string;
  is_perfective: bool;
  is_transitive: bool;
  oblique_object: oblique_object;
  first_prefix: FirstPrefix.t option;
  preformative: Preformative.t option;
  coordinator: bool;  (* coordinator *)
  ventive: bool;      (* ventive prefix *)
  middle_prefix: bool; (* Middle prefix or the 3.SG.NH pronominal prefix *)
  initial_person_prefix: InitialPersonPrefix.t option;
  indirect_object_prefix: IndirectObjectPrefix.t option;
  comitative: bool;   (* comitative prefix *)
  adverbial: adverbial_prefix option;
  locative: locative_prefix option;
  final_person_prefix: FinalPersonPrefix.t option;
  ed_marker: bool; (* ed_marker *)
  final_person_suffix: FinalPersonSuffix.t option;
  subordinator: bool; (* subordinator *)
}