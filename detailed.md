# Detailed Instructions

The following assumes you have installed 
the [prerequisites](getting-started.md#prerequisites) 
and [obtained the code](getting-started.md#getting-the-code).

## Core (Section 6)

<<<<<<< Updated upstream
The code in [`storm-core`](storm-core/) contains the formalization 
of `Storm`'s core API as described in Section 6 of the paper. 
To verify do:
=======
The code in [`storm-core`](storm-core/) contains the formalization of Storm's core API as described in Section 6 of the paper.
To verify the proof do:
>>>>>>> Stashed changes


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

## Policies (Section 7.1)

The code in [`models/`](models/) contains the policies ported 
to evaluate expressiveness as described in Section 7.1.
This directory does not contain verifiable code, only the ported models files.
The models files are grouped by the original tool they were taken from, e.g., the
models file for the Calendar application in UrWeb is in [`models/src/UrWeb/Calendar/Model.storm`](models/src/UrWeb/Calendar/Model.storm).

## Case Studies (Section 7.2)

<<<<<<< Updated upstream
The case studies used to evaluate the burden Storms's puts on programmers as described in Section 7.2 can be found [here](case-studies/).
=======
The case studies used to evaluate the burden Storms's puts on programmers as described in Section 7.2
can be found in [case-studies](case-studies/).
There is one Haskell/Stack project for each case study.
>>>>>>> Stashed changes

### Verify the Code

To verify one of the case studies go to the corresponding directory and build the project with `stack build --fast`, e.g., to verify the WishList application:

```bash
$ cd case-studies/wishlist
$ stack build --fast
```

### Breaking the Code

Open the file [case-studies/wishlist/src/Controllers/Wish.hs](case-studies/wishlist/src/Controllers/Wish.hs).
The function [`getWishData`](case-studies/wishlist/src/Controllers/Wish.hs#L156) at line 156 extracts
the information out of a wish.
The query [between lines 164 and 171](case-studies/wishlist/src/Controllers/Wish.hs#L156-171) checks
if the viewer is friends with the owner of the wish.
Remove the check `frienshipStatus ==. "accepted"` from the query, i.e., the query should look like:

```haskell
  friends  <- selectFirst
    (   friendshipUser1'
    ==. owner
    &&: friendshipUser2'
    ==. viewerId
    )
```

Then verify the code again by running `stack build --fast`.
Forgetting to check if the friendship status is `"accepted"` reports a possible leak because the
viewer may not be friends with the owner of the wish.
You should see an error like:

```
[7 of 8] Compiling Controllers.Wish

**** LIQUID: UNSAFE ************************************************************

/home/artifact/artifact/case-studies/wishlist/src/Controllers/Wish.hs:173:49: error:
    Liquid Type Mismatch
    .
    The inferred type
      VV : {v : (Database.Persist.Class.PersistEntity.Entity Model.User) | (GHC.Types.False => friendshipUser2Cap (getJust (entityKey v)))
                                                                           && (GHC.Types.False => wishOwnerCap (getJust (entityKey VV)))
                                                                           && (GHC.Types.False => friendshipUser1Cap (getJust (entityKey v)))
                                                                           && v == getJust (entityKey v)}
    .
    is not a subtype of the required type
      VV : {VV : (Database.Persist.Class.PersistEntity.Entity Model.User) | wishAccessLevel (entityVal (getJust (entityKey VV))) == "public"
                                                                            || (wishOwner (entityVal (getJust (entityKey VV))) == entityKey VV
                                                                                || (wishAccessLevel (entityVal (getJust (entityKey VV))) == "friends"
                                                                                    && friends (wishOwner (entityVal (getJust (entityKey VV)))) (entityKey VV)))}
    .
    in the context
      wishId : (Database.Persist.Class.PersistEntity.Key Model.Wish)
    |
173 |     (_, Just _) | level == "friends" -> project wishDescription' wish
    |                                                 ^^^^^^^^^^^^^^^^
```

### Figure 9

To produce the count of lines of code in Figure 9. do:

```bash
$ python3 fig9.py
```

## Disco

### Verify the Code

To verify Disco's server code is leak free do:

```bash
$ cd disco/server
$ stack build --fast
```

### Break the Code

Open the file [disco/server/src/Controllers/Room](https://github.com/storm-framework/disco/blob/50d1ff79e76013fc6a018f3bea554508c60e06d8/server/src/Controllers/Room.hs).
The function [`updateTopic`](https://github.com/storm-framework/disco/blob/50d1ff79e76013fc6a018f3bea554508c60e06d8/server/src/Controllers/Room.hs#L36) on line 36 implements
the functionality that allows a user to update a room's topic.
If not done carefully, this operation may produce a subtle information flow bug as described in the
discussion of Section 7.3.
In [line 42](https://github.com/storm-framework/disco/blob/50d1ff79e76013fc6a018f3bea554508c60e06d8/server/src/Controllers/Room.hs#L42) it checks that the users' visibility is set to `"public"` and only then
allows them to update the topic.

Update [lines 42 to 50](https://github.com/storm-framework/disco/blob/50d1ff79e76013fc6a018f3bea554508c60e06d8/server/src/Controllers/Room.hs#L42-50) to be

```haskell
    Just roomId -> do
      UpdateTopicReq {..} <- decodeBody
      validateTopic updateTopicReqTopic
      _ <- updateWhere (roomId' ==. roomId) (roomTopic' `assign` updateTopicReqTopic)
      room                <- selectFirstOr notFoundJSON (roomId' ==. roomId)
      roomData            <- extractRoomData room
      respondJSON status200 roomData
    Nothing -> respondError status403 Nothing
```

and then run `stack build --fast` again.
Forgetting to check if the visibility is set to public produces an error when accessing
the user's current room as the information may be leaked.
You should see an output like:


```
**** LIQUID: UNSAFE ************************************************************
/home/artifact/artifact/disco/server/src/Controllers/Room.hs:39:23: error:
    Liquid Type Mismatch
    .
    The inferred type
      VV : {v : (Database.Persist.Class.PersistEntity.Entity Model.User) | v == getJust (entityKey v)}
    .
    is not a subtype of the required type
      VV : {VV : (Database.Persist.Class.PersistEntity.Entity Model.User) | userVisibility (entityVal (getJust (entityKey VV))) == "public"
                                                                            || getJust (entityKey VV) == VV}
    .
   |
39 |   userRoom <- project userRoom' viewer
   |                       ^^^^^^^^^
```

### Run the Code

There is a running instance of Disco at [http://34.72.16.85:8080](http://34.72.16.85:8080).
We included credentials to log in into the site in the `Bidding instructions/requirements` section of the artifact submission page.
To test the Disco:

1. Log in with the organizer account. This account has administration capabilities.
1. Go to `Admin -> Manage Rooms` at the top right.
1. Create a couple of rooms. You can change the color, name, topic and capacity. When you are ready, click on save.
1. In a different browser (or in an incognito window) log in with the attendee account.
1. You should see the organizer user in the Lobby.
1. Try joining different rooms with both users. The interface should show which room the other user is currently in.

## Voltron

### Verify the Code

```bash
$ cd voltron/server
$ stack build --fast
```

### Break the Code

Open the file [voltron/server/src/Controllers/Class.hs](https://github.com/storm-framework/voltron/blob/1ccdac06802015bf97044e932c8545516eeb7225/server/src/Controllers/Class.hs).
The function [`addRoster`](https://github.com/storm-framework/voltron/blob/1ccdac06802015bf97044e932c8545516eeb7225/server/src/Controllers/Class.hs#L102) at line 102 implements
the functionality to enroll a list of students to a class.
This operation is restricted to instructors of the class which is checked by the [query at line 110](https://github.com/storm-framework/voltron/blob/1ccdac06802015bf97044e932c8545516eeb7225/server/src/Controllers/Class.hs#L110).
Removing the clause `&&: classInstructor' ==. instrId`, i.e., so the line reads:

```haskell
                         (className' ==. rosterClass)
```

should produce an error stating that the user does not have enough permissions for the operation.
You should see the following error:

```
[ 9 of 11] Compiling Controllers.Class

**** LIQUID: UNSAFE ************************************************************

/home/artifact/artifact/voltron/server/src/Controllers/Class.hs:64:25: error:
    Liquid Type Mismatch
    .
    The inferred type
      VV : {v : (Database.Persist.Class.PersistEntity.Entity Model.Class) | isInstructor (entityKey v) (classInstructor (entityVal v))
                                                                            && v == getJust (entityKey v)}
    .
    is not a subtype of the required type
      VV : {VV : (Database.Persist.Class.PersistEntity.Entity Model.Class) | classEditorLangCap VV}
    .
   |
63 |                        (classEditorLang' `assign` cliLanguage)
   |                         ^^^^^^^^^^^^^^^^

```
### Run the Code

We have a running instance of Voltron at [http://34.72.16.85:9090](http://34.72.16.85:9090).
We included credentials to log in into the site in the artifact submission page.
There are 5 account: 1 instructor account and 4 students account.
The students accounts are divided into 2 groups of 2 students each.
If logged in with the instructor account, you should see a _buffer_ for each of the two groups.
When logged in with a student account, you should only see the buffer for that student's group.

