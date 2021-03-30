# Detailed Instructions

The following assumes the steps in [Installing Prerequisites](getting-started.md#installing-prerequisites) and
[Getting the code](getting-started.md#getting-the-code) have been completed.

## Core (Section 6)

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

## Policies (Section 7.1)

The code in [`models/`](models/) contains the policies ported to evaluate expressiveness as described in Section 7.1.
This directory does not contain verifiable code, only the ported models files.
The models files are grouped by the original tool they were taken from, e.g., the
models file for the Calendar application in UrWeb is in [`models/src/UrWeb/Calendar/Model.storm`](models/src/UrWeb/Calendar/Model.storm).

## Case Studies (Section 7.2)

The case studies used to evaluate the burden Storms's puts on programmers as described in Section 7.2
can be found [here](case-studies/).

### Verify the Code

To verify an application go to the corresponding directory and build the project with `stack build --fast`, e.g., to verify the WishList application:

```bash
$ cd case-studies/wishlist
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


## Disco

### Verify the Code

To verify Disco's server code is leak free do:

```bash
$ cd disco
$ stack build --fast
```

### Break the Code

Open the file [disco/server/src/Controllers/Room](https://github.com/storm-framework/disco/blob/50d1ff79e76013fc6a018f3bea554508c60e06d8/server/src/Controllers/Room.hs).
The function [`updateTopic`](https://github.com/storm-framework/disco/blob/50d1ff79e76013fc6a018f3bea554508c60e06d8/server/src/Controllers/Room.hs#L36) on line 36 is the
controller that allows a user to update a room's topic which contained the subtle information flow bug
described in the discussion of Section 7.3.
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

We have a running instance of Disco at [http://34.72.16.85:8080](http://34.72.16.85:8080).
We included credentials to log in into the site in the artifact submission page.
To test the Disco:

1. Log in with the organizer account. This account has administration capabilities.
1. Go to `Admin -> Manage Rooms` at the top right.
1. Create a couple of rooms. You can change the color, name, topic and capacity. When you are ready, click on save.
1. In a different browser or in an incognito window log in with the attendee account.
1. You should see the organizer user in the Lobby.
1. Try joining different rooms with both users. The interface should show which room the other user is currently in.

## Voltron

### Verify the Code

```bash
$ cd voltron
$ stack build --fast
```

### Break the Code

Open the file [voltron/server/src/Controllers/Class.hs](https://github.com/storm-framework/voltron/blob/1ccdac06802015bf97044e932c8545516eeb7225/server/src/Controllers/Class.hs).
The function [`addRoster`](https://github.com/storm-framework/voltron/blob/1ccdac06802015bf97044e932c8545516eeb7225/server/src/Controllers/Class.hs#L102) at line 102 is
the controller that allows an instructor to enroll a list of students to a class.
The [query at line 110](https://github.com/storm-framework/voltron/blob/1ccdac06802015bf97044e932c8545516eeb7225/server/src/Controllers/Class.hs#L110) checks that the current user is the instructor of the class.
By removing the clause `&&: classInstructor' ==. instrId`, i.e., so the line reads:

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
If logged in with the instructor account you should see a _buffer_ for each of the two groups.
When logged in with a student account, you should only see the buffer for the student's group.

