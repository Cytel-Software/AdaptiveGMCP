---
applyTo: "**/*.R"
description: This file describes the R coding conventions for the project, including naming conventions, file organization, function structure, variable naming, syntax rules, and AI code generation guidelines.
---

# BioPharmSoft R Programming Style Guide — AI Context Document

> **Source:** [BioPharmSoft R Style Guide](https://biopharmsoftgrp.github.io/BioPharmSoftRStyleGuide/)
> **Authors:** J. Kyle Wathen, Fei Chen, et al.
> **Purpose:** Authoritative reference for AI-assisted R code generation in the Cytel biopharmaceutical platform.
> **Last Updated:** 2026-03-27

---

## 1. Guiding Principles

1. **Readability over efficiency.** When choosing between a highly efficient but hard-to-follow approach and a less efficient but easily understood approach, ALWAYS choose the readable option. The efficient version can be provided as an alternative with comparison of results for testing.
2. **Descriptive and meaningful names** for files, functions, and variables.
3. **Keep files short.** Extremely long files are difficult to follow and test.
4. **Consistency.** Pick a style option and be consistent within a project, package, or Shiny app.
5. **Collaborative readability.** Software source should read like a book, not a file of "code" intended to be difficult to understand.

---

## 2. Files

### 2.1 File Naming

| Rule | Convention |
|------|-----------|
| Case | **BigCamelCase** (PascalCase) |
| Extension | Always `.R` (capital R) |
| Characters | Letters and numbers only — NO spaces, `-`, `_`, or `.` |
| Content | Name should describe what the file contains |

```r
# GOOD
CalculatePosteriorProbs.R
AnalyzeSurvivalData.R
InputFunctions.R

# BAD
calculate.r          # lowercase extension, vague name
foo.r                # non-descriptive
get file name.r      # spaces in name
functions for stuff.r
```

#### Ordered File Sequences

When files must be viewed or executed in order, prefix with two-digit numbers starting at `00`:

```r
00_SimulateData.R
01_SimulateArrivalTimes.R
02_CreateDataSet.R
09_LogisticRegressionModel.R
10_BetaBinomialModel.R
```

> **Note:** Do not create files that differ only by capitalization (cross-platform compatibility).

### 2.2 File Organization

- Group related functionality into meaningful files.
- If a file contains many functions and is very long, break it into smaller files grouped by common functionality.

### 2.3 Internal Structure — Section Comments

Use section comment dividers with `#` followed by `-`, `#`, or `=` to create logical sections. **Each comment must end with 4 consecutive delimiter characters** to enable the RStudio document outline.

**Critical:** Add a trailing space before the last delimiter character to prevent `(Untitled)` entries in the RStudio outline.

```r
# GOOD — trailing space before final character creates proper outline entries

#------------------------------------------------------ -
# Add2 - This function adds 2 to x
#------------------------------------------------------ -
Add2 <- function( x )
{
    return( x + 2 )
}

#------------------------------------------------------ -
# Subtract2 - This function subtracts 2 from x
#------------------------------------------------------ -
Subtract2 <- function( x )
{
    return( x - 2 )
}
```

```r
# BAD — solid unbroken line creates (Untitled) in outline

#-------------------------------------------------------
# Add2 - This function adds 2 to x
#-------------------------------------------------------
```

Alternative section comment styles (all valid):

```r
# Create simulated data ------
# Plot results ====
# Save output ####
```

---

## 3. Functions

### 3.1 Naming

| Rule | Convention |
|------|-----------|
| Case | **BigCamelCase** (PascalCase) |
| Characters | Letters and numbers only — NO `-`, `_`, or `.` |
| Content | Use **verbs** that describe the action performed |
| S3 methods | `.` is acceptable ONLY for S3 method dispatch (e.g., `ComputePosteriorParameters.BetaBinom()`) |

```r
# GOOD
ComputePosteriorParameters()
SimulatePatientArrivalTimes()
ComputePosteriorParameters.BetaBinom()  # Only if ComputePosteriorParameters is a generic

# BAD
post.par()        # Non-descriptive, dot in non-S3 context
arrival-time()    # Hyphen not allowed
addhr()           # No capitalization, vague
PerformTasks()    # Too generic
Com()             # Too abbreviated
```

### 3.2 Structure

Functions must be **self-contained** and depend ONLY on their arguments. They must NOT use variables from a higher scope. A call with the same arguments must always return the same value (unless the function intentionally produces random variables).

```r
# VERY BAD — result depends on external variable y
MyFunction <- function( x )
{
    x <- x + y
    return( x )
}

# GOOD — depends only on arguments
MyFunction2 <- function( x, y )
{
    x <- x + y
    return( x )
}
```

#### Brace Style

Place opening `{` on the line after `function(...)`, aligned with the first letter of the function name. The closing `}` aligns with the opening `{`.

```r
# Preferred style
MyFunction <- function( x, y )
{
    return( x + y )
}
```

One-liner function definitions are acceptable if the line is short and simple, but the multi-line form is preferred for readability and testing.

### 3.3 Explicit Returns

**ALWAYS use `return()` explicitly.** Do NOT rely on R's implicit return feature.

```r
# GOOD — explicit return
MyFunction <- function( x, y )
{
    return( x + y )
}

# BAD — implicit return is confusing and error-prone
MyFunction <- function( x, y )
{
    x + y
}
```

### 3.4 Local Functions

**Avoid defining functions inside other functions.** Local functions are difficult to test, understand, and prone to errors. Extract them as standalone functions.

```r
# GOOD — separate testable functions
RunAnalysis1 <- function( dfData )
{
    return( lm( dfData$y ~ dfData$x ) )
}

RunAnalysis2 <- function( dfData )
{
    return( lm( dfData$y ~ dfData$x + dfData$x * dfData$Trt ) )
}

RunAnalysis3 <- function( dfData )
{
    return( lm( dfData$y ~ dfData$x + dfData$x2 ) )
}

AnalyzeData <- function( strType, dfData )
{
    if( strType == "ONE" )   fit <- RunAnalysis1( dfData )
    if( strType == "TWO" )   fit <- RunAnalysis2( dfData )
    if( strType == "THREE" ) fit <- RunAnalysis3( dfData )

    return( fit )
}

# BAD — local function definitions are untestable
AnalyzeData <- function( strType, dfData )
{
    if( strType == "ONE" )   ana <- function( dfData ){ lm( dfData$y ~ dfData$x ) }
    if( strType == "TWO" )   ana <- function( dfData ){ lm( dfData$y ~ dfData$x + dfData$x * dfData$Trt ) }
    if( strType == "THREE" ) ana <- function( dfData ){ lm( dfData$y ~ dfData$x + dfData$x2 ) }

    fit <- ana( dfData )
    return( fit )
}
```

> **Improvement tip:** Consider S3 classes where `class(dfData)` determines which analysis to call, eliminating `if` statements.

---

## 4. Variables

### 4.1 Naming Convention

| Rule | Convention |
|------|-----------|
| Case | **camelCase** (first letter lowercase) |
| Characters | Letters and numbers only — NO `-`, `_`, or `.` |
| First letter | **Lowercase** (uppercase is reserved for function names) |

### 4.2 Hungarian Notation Prefixes (Strongly Encouraged)

Since R does not require type declarations, use these prefixes to communicate the intended type:

| Prefix | Type | Example |
|--------|------|---------|
| `n` | Integer | `nQtyOfReps`, `nQtyOfPats` |
| `d` | Double / Float | `dMean`, `dStdDev` |
| `b` | Logical (TRUE/FALSE) | `bSingleArm`, `bAdjust` |
| `v` | Vector | `vMeans`, `vSampleMeans` |
| `m` | Matrix | `mVarCov` |
| `df` | Data frame | `dfPats`, `dfData` |
| `l` | List | `lData`, `lResults` |
| `c` | Class variable | `cAnalysis` (via `structure(list(), class = "TTest")`) |
| `str` | String / Character | `strName`, `strGroup` |

```r
# GOOD — type is immediately clear
nQtyOfPatients <- 100
vSampleMeans   <- c( 1.2, 3.4, 5.6 )
dStdDev        <- 2.5
dfPats         <- data.frame( id = 1:10, trt = rep( c("A","B"), 5 ) )
bAdjust        <- TRUE
strGroup       <- "Treatment"

# BAD — type is ambiguous
xxx
sd
x.m.3
x.mad
```

### 4.3 Loop Variables

Single-letter loop variables are acceptable but **meaningful loop names are preferred**, especially in complex loops:

```r
# Acceptable
for( i in vPatients ){
    vTreatment[ i ] <- vPatients[ i ]
}

# Better — clearer in complex code
for( iPat in vPatients ){
    vTreatment[ iPat ] <- vPatients[ iPat ]
}
```

### 4.4 Abbreviations

Common abbreviations are acceptable if used **consistently** within a repository:

| Abbreviation | Meaning |
|-------------|---------|
| `Qty` or `Quant` | Quantity |
| `Pat` or `Pats` | Patients |
| `Std` | Standard |
| `Dev` | Deviation |
| `Sim` | Simulation |
| `Prob` or `Probs` | Probability / Probabilities |

> **Recommendation:** Maintain a project-level abbreviation list so all developers use consistent naming.

---

## 5. Syntax

### 5.1 Spacing

Put spaces around **all** arithmetic and logical operators, and after every `,`.

Preferred: spaces inside parentheses `( )` and brackets `[ ]` for better readability, especially with multiple arguments.

```r
# BAD — cramped, hard to read
m=1
s=2
x=rnorm(1000,mean=m,sd=s)

# GOOD — descriptive names, proper spacing
dMean <- 1
dSd   <- 2
nSize <- 1000
vSamp <- rnorm( n = nSize, mean = dMean, sd = dSd )
```

```r
# Acceptable
x[i,]
f(x)

# Better
x[i, ]

# Best (preferred)
x[ i, ]
f( x )
```

### 5.2 Code Blocks

Organize code to reduce repetition and increase abstraction. Extract repeated logic into functions.

```r
# BAD — repeated logic
a <- sin( 1.2 ) + exp( 5.7 )
b <- sin( 2.4 ) + exp( -2 )
c <- a + b

# GOOD — abstracted into a function
f <- function( x, y )
{
    return( sin( x ) + exp( y ) )
}

a <- f( 2.4, -2 )
b <- f( 1.2, 5.7 )
c <- a + b
```

### 5.3 Assignment

**Use `<-` for assignment.** Do not use `=`, `->`, or `->>`.

```r
# GOOD
dMean <- 5
vResults <- c( 1, 2, 3 )

# BAD
dMean = 5         # Confuses assignment with argument passing
5 -> dMean        # Rightward assignment is non-standard
5 ->> dMean       # Avoid
```

`<<-` is acceptable only for intentional assignment to parent scope (use sparingly).

> **Note:** The pipe operator `%>%` lends to obfuscation and is **discouraged**.

### 5.4 Semicolons

**Never** use `;` at the end of a line. **Never** put multiple commands on one line separated by `;`.

```r
# GOOD
x <- 5
y <- 2
z <- 3

# BAD
x <- 5; y <- 2; z <- 3;
```

### 5.5 Comments

- Comments should be **meaningful** and add understanding.
- If your code requires a comment on almost every line, **rewrite the code** to be clearer.
- Use descriptive naming conventions, organized files, and functions for complex code blocks instead of heavy commenting.

### 5.6 Formatting Tools

Use the `formatR` or `styler` packages to manage spacing and indentation.

---

## 6. GitHub Conventions

### 6.1 Commit Messages

- First line: brief **subject** description of what the commit does
- If more detail is needed: skip a line, then include body content
- Reference issues with `Fix #<issue-number>` to auto-close on merge

### 6.2 Pull Requests

- Title: brief description of what the PR does
- Include `Fixes #<issue-number>` to auto-close issues

### 6.3 Branch Naming

Format: `<Label>-<Keywords>-<Developer Initials>`

| Label | Purpose |
|-------|---------|
| `Feature` | New features |
| `Bugfix` | Bug fixes |
| `Doc` | Documentation |

```
# Examples
Feature-Add-Analysis-KW
Feature14-Add-Analysis-KW      # References issue #14
Bugfix-Survival-Calc-FC
Doc-Update-Readme-KW
```

#### Branching Strategy

- `main` — production branch
- `Dev` (or `Dev-V0.1.2`) — default development branch
- Feature branches created off `Dev`
- PRs merge feature branches → `Dev`
- After release completion, `Dev` merges → `main` via PR
- Code reviews required for merges to `main` and `Dev`

---

## 7. Quick Reference — Naming Summary

| Element | Case | Prefix | Example |
|---------|------|--------|---------|
| **File** | BigCamelCase | Optional `##_` for ordering | `CalculatePosteriorProbs.R` |
| **Function** | BigCamelCase | Verb-based | `SimulatePatientArrivalTimes()` |
| **Variable (integer)** | camelCase | `n` | `nQtyOfPats` |
| **Variable (double)** | camelCase | `d` | `dMean` |
| **Variable (logical)** | camelCase | `b` | `bSingleArm` |
| **Variable (vector)** | camelCase | `v` | `vSampleMeans` |
| **Variable (matrix)** | camelCase | `m` | `mVarCov` |
| **Variable (data frame)** | camelCase | `df` | `dfPats` |
| **Variable (list)** | camelCase | `l` | `lData` |
| **Variable (class)** | camelCase | `c` | `cAnalysis` |
| **Variable (string)** | camelCase | `str` | `strName` |
| **Loop variable** | camelCase | Meaningful prefix | `iPat`, `iSim` |
| **Git branch** | Kebab-ish | `Feature-`/`Bugfix-`/`Doc-` | `Feature-Add-Analysis-KW` |

---

## 8. AI Code Generation Rules

### DO

- Use `BigCamelCase` for file names and function names
- Use `camelCase` with Hungarian notation prefixes for variables
- Use `<-` for assignment
- Use `return()` explicitly in every function
- Place `{` and `}` on their own lines, aligned with the function name
- Add spaces around operators and after commas
- Prefer spaces inside parentheses: `f( x, y )` not `f(x, y)`
- Keep functions self-contained — depend only on arguments
- Write section comment dividers with trailing space: `#----- -`
- Keep files short and focused on related functionality
- Use meaningful, descriptive names that read like prose
- Extract repeated logic into standalone functions
- Use S3 dispatch patterns where appropriate to eliminate `if` chains

### DON'T

- Don't use `.` in function names unless defining S3 methods
- Don't use `_`, `-`, `.`, or spaces in file names, function names, or variable names
- Don't use `=` for assignment (only use in function arguments: `mean = dMean`)
- Don't use implicit returns — always wrap in `return()`
- Don't use `;` to terminate lines or combine statements
- Don't define functions inside other functions (local functions)
- Don't reference variables from a higher scope inside functions
- Don't use the pipe operator `%>%` (considered obfuscating)
- Don't use `->` or `->>` for assignment
- Don't use single-letter variable names outside of simple loops
- Don't create vague names like `foo`, `stuff`, `xxx`, `sd`, `x.m.3`
- Don't write excessively long files — split into focused modules

---

## 9. Reference Links

- [BioPharmSoft R Style Guide](https://biopharmsoftgrp.github.io/BioPharmSoftRStyleGuide/)
- [Google R Style Guide](https://google.github.io/styleguide/Rguide.html)
- [Tidyverse Style Guide](https://style.tidyverse.org/)
- [GitHub Repository](https://github.com/kwathen/BioPharmSoftRStyleGuide)
