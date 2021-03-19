# Artifact Accompanying the STORM Paper


## TODO / Contents

### Proof

- [] Core

### Q1 

- [] Ported policies (ur, hails etc)
	- [] Scripts to generate table


### Q2 

- [] Ported apps 
	- how to build
	- how to edit to break some policy

### Q3 

- [] Voltron
	- how to build
	- how to edit to break some policy
	- how to run

- [] Disco 
	- how to build
	- how to edit to break some policy
	- how to run

- [] Scripts to generate table

### Packaging

- [] docker or VM?

## Core (Section 5)

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
