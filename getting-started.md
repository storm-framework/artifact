# Getting Started


## Installing Prerequisites

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

## Getting the Code

Clone this repo recursively.

```bash
$ git clone --recursive https://github.com/storm-framework/artifact
```

## Building the Code


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

## Testing applications

We deployed Disco and Voltron to test their functionality.
Try to log in with the credentials provided in the artifact submission site to check everything
is working:

- Disco: http://34.72.16.85:8080
- Voltron: http://34.72.16.85:9090
