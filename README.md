# Artifact Accompanying the STORM Paper

## Getting Started


### Installing Prerequisites

This section contains instructions to install the software required to evaluate the artifact.
If using the provided remote machine, this section can be skipped as the required software is
preinstalled.
Credentials to access the remote machine can be found in the artifact submission site.

- [stack v2.5.1](https://docs.haskellstack.org/en/stable/README/) — To install stack follow instructions [here](https://docs.haskellstack.org/en/stable/README/#how-to-install).

- [z3 v4.8.10](https://github.com/Z3Prover/z3) — A copy of the z3 binary needs to be in the path.
Precompiled binaries can be found [here](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.10).
The package found in GitHub also contains shared libraries and bindings for Python and Java which can be ignored.
It is only necessary to move the `z3` binary to somewhere in the path.

- [tokei v12.1.2](https://github.com/XAMPPRocky/tokei) (optional) — We use tokei to count lines of client code.
    If tokei is not installed the script to compute Figure 9 would still work, but it won't count client code.
    Instructions to install tokei can be found [here](https://github.com/XAMPPRocky/tokei#installation).

### Getting the Code

Clone this repo recursively.

```bash
$ git clone --recursive https://github.com/storm-framework/artifact
```

### Building the Code


```bash
$ cd case-studies/wishlist
$ stack build --fast
```

This will build and verify the code for the WishList application along with its dependencies.
The remote machine has the dependencies precompiled which should speed the process.
If everything goes smoothly, you should see the following at the end of the output.


```
Building library for whishlist-0.1.0.0..
[1 of 8] Compiling Helpers

**** LIQUID: SAFE (0 constraints checked) **************************************
[2 of 8] Compiling Lib

**** LIQUID: SAFE (0 constraints checked) **************************************
[3 of 8] Compiling Model

**** DONE:  Only compiling specifications [skipping verification] **************


**** LIQUID: SAFE (0 constraints checked) **************************************
[4 of 8] Compiling Controllers

**** LIQUID: SAFE (1 constraints checked) **************************************
[5 of 8] Compiling Controllers.Wish

**** LIQUID: SAFE (1510 constraints checked) ***********************************
[6 of 8] Compiling Controllers.User

**** LIQUID: SAFE (532 constraints checked) ************************************
[7 of 8] Compiling Paths_whishlist

**** LIQUID: SAFE (0 constraints checked) **************************************
[8 of 8] Compiling Server

**** DONE:  Only compiling specifications [skipping verification] **************


**** LIQUID: SAFE (0 constraints checked) **************************************

```

### Testing applications

We deployed Disco and Voltron to test their functionality.
Try to log in with the credentials provided in the artifact submission site to check everything
is working:

- Disco: http://34.72.16.85:8080
- Voltron: http://34.72.16.85:9090

## Detailed Instructions

The following assumes the steps in [Installing Prerequisites](#installing-prerequisites) and
[Getting the code](#getting-the-code) have been completed.

### Core (Section 6)

The code in [`storm-core`](storm-core/) contains the formalization of Storm's core API as described in Section 6 of the paper.
To verify do:


```bash
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

### Policies (Section 7.1)

The code in [`models/`](models/) contains the policies ported to evaluate expressiveness as described in Section 7.1.
As described in the paper this directory does not contain verifiable code, but only ported models files.
The models files are grouped by the original tool they were taken from, e.g., the
models file for the Calendar application in UrWeb is in [`models/src/UrWeb/Calendar/Model.storm`](models/src/UrWeb/Calendar/Model.storm).

### Case Studies (Section 7.2)

The case studies used to evaluate the burden Storms's puts on programmers as described in Section 7.2
can be found [here](case-studies/).
To verify an application go to the corresponding directory and build the project with `stack build --fast`, e.g., to
verify the Conference Manager do:

```bash
$ cd case-studies/course
$ stack build --fast
```

You should see output like this (after compiling the dependencies):

```
Building library for course-0.1.0.0..
[1 of 8] Compiling Lib

**** LIQUID: SAFE (0 constraints checked) **************************************
[2 of 8] Compiling Model

**** DONE:  Only compiling specifications [skipping verification] **************


**** LIQUID: SAFE (0 constraints checked) **************************************
[3 of 8] Compiling Helpers

**** LIQUID: SAFE (0 constraints checked) **************************************
[4 of 8] Compiling Controllers

**** LIQUID: SAFE (20 constraints checked) *************************************
[5 of 8] Compiling Controllers.SubmissionShow

**** LIQUID: SAFE (484 constraints checked) ************************************
[6 of 8] Compiling Controllers.CourseIndex

**** LIQUID: SAFE (260 constraints checked) ************************************
[7 of 8] Compiling Controllers.AssignmentShow

**** LIQUID: SAFE (167 constraints checked) ************************************
[8 of 8] Compiling Paths_course

**** LIQUID: SAFE (0 constraints checked) **************************************

```

### Figure 9

To produce the count of lines of code in Figure 9. do:

```bash
$ python3 fig9.py
```


### Disco

- Verify Disco's server code:
```bash
$ cd disco
$ stack build --fast
```




## Structure

- [`storm-core`](storm-core/) — Formalization of Storm's core API as described in Section 6 of the paper.
- [`models`](models/) — Policies ported to evaluate expressiveness as described in Section 7.1.
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

