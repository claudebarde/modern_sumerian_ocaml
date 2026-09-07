/* Shared test scaffolding for building and printing verb forms step by step.
   Reused across the test files in this folder. */

/* One constructor per Conjugator setter exercised by these tests.
   Add a case here (and to `apply_step` below) whenever a test needs
   a prefix/marker that isn't covered yet. */
type steps =
  | Perfective
  | Imperfective(option(Conjugator.ipfv_stem))
  | Transitive
  | Intransitive
  | Subject(Conjugator.PersonParam.t)
  | Object(Conjugator.PersonParam.t)
  | IndirectObject(Conjugator.PersonParam.t)
  | ObliqueObject(Conjugator.PersonParam.t)
  | Ventive
  | MiddlePrefix
  | ModalGa
  | ModalHa
  | Negative
  | Ablative(option(Conjugator.PersonParam.t))
  | Terminative(option(Conjugator.PersonParam.t))
  | Comitative(option(Conjugator.PersonParam.t))
  | LocativeIn(option(Conjugator.PersonParam.t))
  | LocativeOn(option(Conjugator.PersonParam.t))
  | Preformative(Conjugator.Preformative.t);

/* Normalizes every Conjugator setter to `result`, since some of them
   (set_subject, set_object) can already fail while others (is_perfective,
   set_ventive, ...) always succeed. */
let apply_step = (verb: Conjugator.t, step: steps): result(Conjugator.t, string) =>
  switch (step) {
  | Perfective => Ok(Conjugator.is_perfective(verb))
  | Imperfective(stem) => Ok(Conjugator.is_imperfective(verb, stem))
  | Transitive => Ok(Conjugator.is_transitive(verb))
  | Intransitive => Ok(Conjugator.is_intransitive(verb))
  | Subject(person) => Conjugator.set_subject(verb, person)
  | Object(person) => Conjugator.set_object(verb, person)
  | IndirectObject(person) => Ok(Conjugator.set_indirect_object(verb, person))
  | ObliqueObject(person) => Ok(Conjugator.set_oblique_object(verb, person))
  | Ventive => Ok(Conjugator.set_ventive(verb))
  | MiddlePrefix => Ok(Conjugator.set_middle_prefix(verb))
  | ModalGa => Ok(Conjugator.set_modalGa(verb))
  | ModalHa => Ok(Conjugator.set_modal(verb, Modal))
  | Negative => Ok(Conjugator.set_negative(verb))
  | Ablative(person) => Ok(Conjugator.set_ablative(verb, person))
  | Terminative(person) => Ok(Conjugator.set_terminative(verb, person))
  | Comitative(person) => Ok(Conjugator.set_comitative(verb, person))
  | LocativeIn(person) => Ok(Conjugator.set_locative_in(verb, person))
  | LocativeOn(person) => Ok(Conjugator.set_locative_on(verb, person))
  | Preformative(preformative) => Ok(Conjugator.set_preformative(verb, preformative))
  };

/* Threads the result through the whole list of steps instead of
   discarding errors: a failing step (e.g. Object on an intransitive verb)
   short-circuits the fold and surfaces in the final output. */
let apply_steps = (verb: Conjugator.t, steps: list(steps)): result(Conjugator.t, string) =>
  steps
  |> List.fold_left(
       (acc, step) =>
         switch (acc) {
         | Error(_) as err => err
         | Ok(verb) => apply_step(verb, step)
         },
       Ok(verb),
     );

/* Builds a verb from its stem, applies every step, then prints it.
   Distinct sentinels for a setter failure vs. a print failure so a
   failing test tells you where in the pipeline it broke. */
let conjugate = (stem: string, steps: list(steps)): string =>
  switch (apply_steps(Conjugator.create(stem), steps)) {
  | Error(err) => "STEP_ERROR: " ++ err
  | Ok(verb) =>
    switch (Conjugator.print(verb, None)) {
    | Ok({verb, _}) => verb
    | Error(err) => "PRINT_ERROR: " ++ err
    }
  };
