open Jest;
open Conjugation_helpers;

describe("Applies the first person singular", () => {
  open Expect;

  test("Third person singular non-human", () => {
    let verbs_to_test = [|
      (
        "shum",
        [
          Transitive,
          Perfective,
          IndirectObject(Conjugator.PersonParam.First_sing),
          Subject(Conjugator.PersonParam.Third_sing_non_human),
        ],
        "mabshum",
      ), // Jagersma 13.2.2 (14)
      (
        "dab",
        [
          Transitive,
          Perfective,
          MiddlePrefix,
          Subject(Conjugator.PersonParam.Third_sing_non_human),
        ],
        "babdab",
      ), // Jagersma 13.2.2 (15)
      (
        "dab",
        [
          Transitive,
          Imperfective(Some(Other("dab"))),
          ModalHa,
          Subject(Conjugator.PersonParam.Third_sing_non_human),
          Object(Conjugator.PersonParam.Third_sing_non_human),
          ObliqueObject(Conjugator.PersonParam.Third_sing_non_human),
        ],
        {js|ḫabibdabe|js},
      ), // Jagersma 13.2.2 (16)
      (
        "tuku",
        [
          Transitive,
          Perfective,
          Subject(Conjugator.PersonParam.Third_sing_non_human),
          Preformative(Conjugator.Preformative.I),
          Negative
        ],
        "nuubtuku",
      )
    |];

    let results =
      verbs_to_test |> Array.map(((stem, steps, _)) => conjugate(stem, steps));

    let expected =
      verbs_to_test |> Array.map(((_, _, expected_form)) => expected_form);

    expect(results) |> toEqual(expected);
  });
});
