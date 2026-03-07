# Monetary Policy and Credit Segmentation in Brazil

## Status
active

## One-line question
Do monetary policy shocks affect free credit and earmarked credit differently in Brazil?

## Motivation
Brazil is a natural setting for studying credit segmentation because its financial system has long combined market-based credit with earmarked or directed credit. If earmarked credit is less sensitive to policy-rate changes, then the credit channel of monetary policy may be weaker or uneven across segments of the economy.

This is attractive because it has:
- a clear institutional story,
- a strong macro-financial mechanism,
- good fit with VAR / SVAR / LP methods,
- and direct policy relevance.

## Why this is interesting
The project connects monetary policy to heterogeneous credit transmission. It is macroeconometric, but the mechanism is microfounded: different borrowers and lenders face different financing conditions, and some credit categories are partially insulated from standard interest-rate transmission.

## Main hypothesis
A contractionary monetary policy shock reduces free credit more strongly than earmarked credit.

## Secondary hypotheses
- The response of total credit is weaker than the response of free credit because earmarked credit dampens aggregate pass-through.
- The difference may be stronger in firm credit than in household credit.
- The difference may vary across subperiods, especially before and after major policy shifts.

## Baseline empirical strategy
Main method: local projections (LP).

For each horizon \( h = 0, 1, \dots, H \), estimate:
\[
y_{t+h} - y_{t-1} = \alpha_h + \beta_h MPShock_t + \Gamma_h X_t + \varepsilon_{t+h}
\]

Where:
- \( y \) is alternatively:
  - free credit growth,
  - earmarked credit growth,
  - total credit growth,
- \( MPShock_t \) is a monetary policy shock,
- \( X_t \) contains controls and lags.

Compare the impulse responses across credit categories.

## Benchmark specification
Run a small VAR / SVAR as a benchmark.

Possible variables:
- policy rate or policy shock,
- inflation,
- activity,
- free credit,
- earmarked credit.

Use recursive identification as a baseline if no better shock measure is available.

## Preferred frequency
Monthly.

## Candidate data
### Monetary policy
- Selic rate
- Monetary policy surprises if available
- Central Bank of Brazil

### Credit
- free credit
- earmarked credit
- total credit
- possibly household vs corporate breakdown
- Central Bank of Brazil monetary and credit statistics

### Macroeconomic controls
- inflation
- industrial production or economic activity index
- exchange rate if needed

## Sample
Likely 2000s onward, depending on consistent credit-series availability.

## Identification concerns
- A Selic change is not automatically an exogenous monetary shock.
- Recursive identification is standard but imperfect.
- Credit aggregates may reflect both supply and demand.
- Structural breaks matter in Brazil.

## What would make the project convincing
- A clean comparison of impulse responses for free vs earmarked credit.
- Transparent identification assumptions.
- A compact, well-motivated system rather than too many variables.
- Robustness across a small VAR and LP.

## Possible extensions
### Extension 1: Corporate vs household credit
Estimate differential responses separately if data permit.

### Extension 2: Subsamples
Compare pre-2015 and post-2015 periods.

### Extension 3: BNDES-specific robustness
Use BNDES-related credit series or discuss BNDES as the key institutional mechanism.

## Why feasible
- Good macro time series are available.
- The question maps naturally into LP and VAR methods.
- The mechanism is easy to explain.
- Suitable for a class paper of roughly 15–25 pages.

## Risks
- Shock identification could become the weakest part.
- Too many robustness exercises could make the paper unfocused.
- Institutional detail may tempt overexpansion.

## Current assessment
This is currently the strongest project idea.

## Next steps
1. Verify the exact Central Bank series for free and earmarked credit.
2. Decide on the monetary policy shock measure.
3. Choose sample period and frequency.
4. Sketch the baseline LP equation.
5. Check two or three key Brazil papers for positioning.

## Rough title options
- Monetary Policy and Credit Segmentation in Brazil
- Do Monetary Policy Shocks Affect Free and Earmarked Credit Differently in Brazil?
- Directed Credit and Monetary Transmission in Brazil: Evidence from Local Projections