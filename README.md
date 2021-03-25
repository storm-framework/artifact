# Artifact Accompanying the STORM Paper

## Prerequisites

- [stack](https://docs.haskellstack.org/en/stable/README/) — To install follow instructions [here](https://docs.haskellstack.org/en/stable/README/#how-to-install).
- [z3](https://github.com/Z3Prover/z3) — LiquidHaskell requires z3 to be in the path. Precompiled binaries can be found [here](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.10).
- [tokei](https://github.com/XAMPPRocky/tokei) (optional) — Tokei is used to count lines in client code.
    If tokei is not installed the script to compute Figure 9 would still work, but it won't count client code.
    To install follow instructions [here](https://github.com/XAMPPRocky/tokei#installation).

## Structure

- [`storm-core`](storm-core/) — Formalization of Storm's core API as described in Section 6 of the paper.
- [`models`](models/) — Policies ported to evaluate expressiveness as described in Section 7.1.
- [`case-studies`](case-studies/) — Case studies to evaluate burden Storms's puts on the programmer as described in Section 7.2 of the paper.
- [`voltron`](voltron/) — The repository hosting the Voltron app described in Section 7.3. This is further divided in [`client`](voltron/client) and [`server`](voltron/server) code.
- [`disco`](disco/) — The repository hosting the Disco app described in Section 7.3. The repo is further divided in [`client`](disco/client) and [`server`](disco/server) code.

## TODO / Contents

### Proof

- [x] **S6** Core

- [ ] **S7.1** Ported policies (ur, hails etc)
	- [ ] Script to generate table 1 (policies)

- [ ] **S7.2** Ported apps
	- how to build
	- how to edit to break some policy

- [ ] **S7.3**
	- Voltron
	  - how to build
	  - how to edit to break some policy
	  - how to run

	- [ ] Disco
	  - how to build
	  - how to edit to break some policy
	  - how to run

- [x] Scripts to generate Figure 9 (annotations)


### Packaging

- [X] Docker or VM? We are giving them access to a server.

## Core (Section 6)

Do this

```
$ cd storm-core
$ stack build --fast
```

You should see something like

```
rjhala@khao-soi ~/r/storm-core (master)> stack build --fast

.
.
.

[1 of 9] Compiling Labels
**** LIQUID: SAFE (6 constraints checked) **************************************

[2 of 9] Compiling LIO
**** LIQUID: SAFE (15 constraints checked) *************************************

[3 of 9] Compiling LIOCombinators
**** LIQUID: SAFE (99 constraints checked) *************************************

[4 of 9] Compiling Lifty
**** LIQUID: SAFE (135 constraints checked) ************************************

[5 of 9] Compiling Paths_storm_core
**** LIQUID: SAFE (0 constraints checked) **************************************

[6 of 9] Compiling ProofCombinators
**** LIQUID: SAFE (1 constraints checked) **************************************

[7 of 9] Compiling Rows
**** LIQUID: SAFE (0 constraints checked) **************************************

[8 of 9] Compiling Storm
**** LIQUID: SAFE (70 constraints checked) *************************************

[9 of 9] Compiling Storm2
**** LIQUID: SAFE (82 constraints checked) *************************************
```
