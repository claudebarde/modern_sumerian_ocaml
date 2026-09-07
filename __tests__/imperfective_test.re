open Jest;

describe("Checks if the imperfective stem is correctly loaded", () => {
  open Expect;

  test("imperfective forms", () => {
    let verbs_to_test = [|
      ("ak", Conjugator.Ed_marker, "aked"),
      ("dab", Conjugator.Reduplicate(None), "dab-dab"),
      ("dug", Conjugator.Reduplicate(Some("du-du")), "du-du"),
      ("e", Conjugator.Other("ed"), "ed")
    |];

    let results =
      verbs_to_test
      |> Array.map(((verb_stem, impf_stem, _expected_imperfective)) => {
           let verb_to_test =
             Conjugator.create(verb_stem)
             ->Conjugator.is_imperfective(Some(impf_stem));

           switch (Conjugator.print(verb_to_test, None)) {
           | Ok({verb, _}) => verb
           | Error(_) => "FAILED_TO_CONJUGATE"
           }
         });

    let expected =
      verbs_to_test
      |> Array.map(((_, _, expected_imperfective)) => expected_imperfective);

    expect(results) |> toEqual(expected)
  })
});