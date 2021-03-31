# Getting Started

## Prerequisites

The artifact requires the following dependencies -- `stack`, `z3` and `tokei`.
This section can be skipped if you are using the provided remote machine, 
as the required software is pre-installed. 
(The necessary credentials for logging into the machine are on the artifact submission site.)


- [stack v2.5.1](https://docs.haskellstack.org/en/stable/README/) 
  Follow [these instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install) to install `stack`.

- [z3 v4.8.10](https://github.com/Z3Prover/z3) 
  A copy of the precompiled `z3` binary [available here](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.10) needs to be in the path. 
  You can ignore the shared libraries and bindings for Java and Python; just
  download and place a suitable `z3` binary somewhere in your `$PATH`.

- [tokei v12.1.2](https://github.com/XAMPPRocky/tokei) (optional) 
  Follow [these instructions](https://github.com/XAMPPRocky/tokei#installation)
  to install `tokei` which is used to count lines of client code. 
  If not installed the script to compute Figure 9 would still work, 
  but it will not count client code.

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

We deployed `Disco` and `Voltron` to test their functionality. You can log in with the credentials provided in the artifact submission site to check everything is working:

- Disco: http://34.72.16.85:8080
- Voltron: http://34.72.16.85:9090

To see more, you can continue with the [detailed instructions](detailed.md).


