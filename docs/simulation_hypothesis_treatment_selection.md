# Simulation Hypothesis and Treatment Selection

This note describes how `simMAMSMEP()` determines which hypotheses and treatment arms proceed from look 1 to look 2. The same selection helper is used by the CER and p-value combination simulation paths.

## Decision sequence

1. **Apply look-1 rejection.** Initially, `HypoPresent` and `IndexSet` contain only hypotheses not rejected at look 1. A treatment arm remains present while at least one hypothesis associated with that arm is unrejected.
2. **Check stopping.** With `MultipleWinners = FALSE`, any rejection stops the trial. With `MultipleWinners = TRUE`, the trial can continue while at least one nondropped hypothesis remains unrejected.
3. **Check whether selection applies.** Selection runs only when `Selection = TRUE` and look 1 is included in `SelectionLook`. Otherwise, all unrejected hypotheses proceed.
4. **Build the candidate set.** The code identifies treatment arms having at least one unrejected hypothesis, then includes all hypotheses on those arms in the candidate set. Consequently, a rejected hypothesis can re-enter the candidate set when another hypothesis on the same arm is unrejected.
5. **Filter by endpoint.** `SelectEndPoint = "overall"` keeps candidates from all endpoints. A numeric `SelectEndPoint` keeps candidates only from that endpoint.
6. **Apply the selection rule.** `SelectionScale` chooses `pvalue`, `stderror`, `delta`, or `teststat`. `SelectionCriterion` then applies `best`, `threshold`, `epsilon`, or `random`, using `SelectionParameter` where applicable.
7. **Optionally retain associated hypotheses.** With `KeepAssociatedHypo = TRUE`, selecting one hypothesis retains all hypotheses on its treatment arm, including hypotheses on other endpoints and potentially hypotheses rejected at look 1.
8. **Finalize look 2.** Hypotheses not selected are marked dropped, arms absent from the selected set are dropped, and an empty selection stops the trial. `ImplicitSSR` controls redistribution of stage-2 sample size; it does not select hypotheses.

A hypothesis rejected at look 1 may therefore receive stage-2 data and appear in `SelectedIndex`, but its prior rejection remains final and its removed closed-testing intersections are not restored.

## Example 1: One endpoint, multiple treatment arms

Suppose one control and three treatments define:

| Hypothesis | Comparison | Look-1 result |
| --- | --- | --- |
| H1 | Treatment 1 vs control | Rejected |
| H2 | Treatment 2 vs control | Not rejected |
| H3 | Treatment 3 vs control | Not rejected |

Because each treatment has only one hypothesis, the candidate set is H2 and H3. H1 cannot re-enter through another endpoint, and `KeepAssociatedHypo` adds nothing.

- If selection is disabled or look 1 is not in `SelectionLook`, H2 and H3 proceed.
- With `SelectionCriterion = "best"`, `SelectionScale = "pvalue"`, and `SelectionParameter = 1`, only the candidate with the smaller p-value proceeds.
- With a threshold or epsilon rule, both, one, or neither may proceed according to `SelectionParameter`.
- An empty selection stops the trial before look 2.

Thus, for one endpoint, rejected hypotheses remain excluded, but selection can still remove some unrejected hypotheses.

## Example 2: Two endpoints, multiple treatment arms

Suppose one control and three treatments define:

| Endpoint 1 | Endpoint 2 | Treatment |
| --- | --- | --- |
| H1 | H4 | Treatment 1 |
| H2 | H5 | Treatment 2 |
| H3 | H6 | Treatment 3 |

Assume H1 and H5 are rejected at look 1, while H2, H3, H4, and H6 are not rejected. Every treatment arm still has at least one unrejected hypothesis, so the arm-based candidate set contains H1 through H6, including rejected H1 and H5.

With `SelectEndPoint = 1`, the selection candidates are H1, H2, and H3. Therefore, a rule such as `SelectionCriterion = "best"` can select rejected H1 if its endpoint-1 selection metric is best.

Alternatively, suppose the rule selects H2:

- With `KeepAssociatedHypo = FALSE`, only H2 proceeds operationally.
- With `KeepAssociatedHypo = TRUE`, both H2 and its endpoint-2 associate H5 are retained, even though H5 was rejected at look 1.

This multi-endpoint arm expansion explains why simulation `SelectedIndex` is not always a subset of the hypotheses that remained active immediately after look 1.
