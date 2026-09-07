# adaptGMCP_CER() api and corresponding non-interactive api
**Purpose**

`cerAdaptGMCP_Analysis.R:29` runs an interactive two-stage adaptive graphical multiple-comparison procedure using the conditional error rate principle.

It supports:

- Multiple treatment arms sharing a control
- Multiple endpoints
- Continuous, binary, or mixed endpoint structures
- Parametric, partly parametric, and non-parametric local tests
- Graphical alpha recycling
- Stage 2 arm selection, sample-size changes, and graph changes

Its primary interface is console prompts, printed tables, and plots. It does not return the final analysis state.

**Hypothesis Structure**

The number of elementary hypotheses is

$$
m = nEps \times (nArms - 1).
$$

`cerConnectedSets.R:65` maps each hypothesis to:

- Its endpoint
- Its endpoint type
- The common control
- Its treatment arm

`generateWeights.R:11` expands the initial graphical procedure into all nonempty intersection hypotheses. For each intersection it records:

- Which elementary hypotheses belong to it
- Their propagated local weights
- The graph obtained after eliminating excluded nodes

This intersection-weight table, `WH`, is the basis of the closed-testing procedure.

**Initialization**

At entry, the function:

1. Sets fixed internal options such as right-tailed testing and common-control comparisons.
2. Enables selection, multiple winners, sample-size modification, and strategy updates internally.
3. Chooses an `mvtnorm` integration algorithm based on the hypothesis count.
4. Builds the hypothesis map and all intersection graphs.
5. Optionally plots the initial graph.
6. Creates a mutable internal list named `mcpObj`.

The important `mcpObj` fields include:

- Current and initial active hypotheses
- Current and previous intersection weights
- Rejected and dropped flags
- Planned and adapted sample sizes
- Stage 1 and adaptation results
- Current p-values
- Hypothesis-to-arm mapping
- Original and modified graph representations

This object is initialized in `cerAdaptGMCP_Analysis.R:107`.

**Stage 1**

For every active hypothesis, `pValueAdaptGmcpHelper.R:214` prompts for a raw p-value. Missing inactive hypotheses are padded with `NA`.

`cerStage1Analysis.R:8` then:

1. Allocates cumulative sample sizes by information fraction.
2. Computes the planned covariance structure for parametric components.
3. Computes planned Stage 1 and Stage 2 boundaries.
4. Applies closed testing at the Stage 1 boundaries.
5. Produces formatted tables for boundaries, intersections, and elementary decisions.

**Boundary Computation**

`cerPlanBoundary.R:8` first obtains the Stage 1 alpha spending from `rpact`.

For every intersection hypothesis, it partitions the active hypotheses into local testing subsets:

- Correlated hypotheses within an endpoint use a weighted parametric test.
- Singleton or non-parametric hypotheses use weighted Bonferroni logic.
- Several such subsets produce a mixed procedure.

It numerically solves for common multipliers $c_{J1}$ and $c_{J2}$, giving weighted boundaries

$$
a_{Ji1} = c_{J1}w_{Ji},
\qquad
a_{Ji2} = c_{J2}w_{Ji}.
$$

The roots are chosen so the relevant boundary-crossing probability equals the allocated Stage 1 alpha or overall alpha.

**Closed Testing**

`cerClosedTest.R:50` rejects an intersection when at least one constituent p-value crosses its local weighted boundary.

An elementary hypothesis is rejected only when every tested intersection containing it is rejected. This is the standard closed-testing requirement and provides the multiplicity control underlying the procedure.

After Stage 1, `adaptGMCP_CER()`:

- Updates elementary rejection flags
- Removes rejected hypotheses from `IndexSet`
- Removes intersections containing already rejected hypotheses
- Preserves the pre-adaptation intersection weights in `WH_Prev`
- Retains the corresponding planned Stage 2 boundaries

**Conditional Error Calculation**

`cerComputation.R:30` computes the conditional probability of a future rejection under the original design, conditional on the observed Stage 1 p-values.

For each remaining intersection:

- Parametric subsets use the planned covariance and conditional multivariate-normal crossing probability.
- Non-parametric subsets use PCER calculations based on Stage 1 and Stage 2 sample sizes.
- Mixed intersections combine these subset contributions.

The CER table is printed after Stage 1, but it is currently held only in the local `CERTab` variable rather than saved in `mcpObj`.

**Interim Decisions**

After displaying Stage 1 results, `pValueAdaptGmcpHelper.R:233` asks whether to:

- Continue to Stage 2
- Terminate
- Re-enter the current look

If continuing with adaptation enabled, the function performs these operations in order:

1. **Selection:** `pValueAdaptGmcpHelper.R:257` asks which hypotheses continue and updates dropped flags, active hypotheses, intersections, and continuing arms.
2. **Sample-size modification:** `cerStage2Analysis.R:120` optionally collects new cumulative Stage 2 sample sizes.
3. **Strategy modification:** `pValueAdaptGmcpHelper.R:306` optionally collects new graph weights and transition edges.
4. **Boundary adaptation:** `cerAdaptBoundary.R:9` recalculates Stage 2 boundaries.

The adapted boundaries are chosen so that, conditional on Stage 1, the modified design expends the same conditional error as the original design.

**Stage 2**

`cerStage2Analysis.R:7` has two paths.

With `AdaptStage2 = FALSE`:

- It uses the planned Stage 2 boundaries.
- It directly applies closed testing to the supplied Stage 2 p-values.

With `AdaptStage2 = TRUE`:

- It calculates the realized information fraction for each continuing hypothesis.
- It combines Stage 1 and incremental Stage 2 p-values:

$$
p_{\mathrm{cum}}
=
1-\Phi\left(
\sqrt{t}\,\Phi^{-1}(1-p_1)
+
\sqrt{1-t}\,\Phi^{-1}(1-p_2)
\right).
$$

- It tests these cumulative p-values against the conditionally adjusted boundaries.
- It preserves previous rejection decisions for hypotheses whose Stage 2 p-values are `NA`.

The function then prints the covariance matrix, adapted test tables, cumulative p-values, final rejection decisions, and optional final graph.

**Termination And Output**

`pValueAdaptGmcpHelper.R:564` considers the trial stopped when:

- All non-dropped hypotheses are rejected, because `MultipleWinners` is fixed to `TRUE`; or
- The current look is the last planned look.

However, termination still goes through a console decision. The caller receives no structured result because the function has no explicit `return(mcpObj)`.

**Important Characteristics For The New API**

- The reusable statistical calculations already live in helpers.
- Interactive input, orchestration, state mutation, printing, and plotting are tightly combined in `adaptGMCP_CER()`.
- Input validation at the public entry point is mostly absent.
- The code derives the number of looks from `info_frac`, but its control flow only distinguishes Look 1 from Stage 2; it does not enforce exactly two looks.
- Empty-selection handling sets `mcpObj$ContTrial`, while the loop actually uses a separate `ContTrial` variable.
- Console input uses `eval(parse(...))`, coupling validation to interactive expression parsing.
- Regression tests capture results by mocking input and continuation helpers because the function itself does not return its state.

Therefore, the new API should preserve these statistical computations and ordering while replacing prompts and hidden mutable state with explicit arguments, returned state, validation, and deterministic completion rules.
