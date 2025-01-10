#  Contributing to elk-herd

This is a big project: 75+ source files, 17k lines of code.  And, I've been
coding this myself since 2017... which means the code probably isn't the
easiest for someone else to wrap their mind around.

I've tried to provide a decent road map to the source code here. And afterwords
some notes about my coding style and what I'd expect from contributions.



# Code Tour

In each section, I've listed the files in a sensible order to read through
them.

**Reading my source code:**

I use comments sparingly. Basically, I'm a fan of "does what it says on the
box", meaning between the types and the function name, you should already
have a good idea of what it does. You might need to look at the argument names.

Many modules have some explanatory comments at the top, but as nothing here is
a package, I generally don't clutter the source with examples of usage. The
application code is full of them! Go see how a function is actually used for
examples.

### Utility types

`Bank` is a type for managing a fixed set of slots that can contain an item.
Used for things like the 128 Pattern slots in a project.

    Bank.elm
    Bank/IndexSet.elm

`ByteArray` handles all the binary data. It is more like `Array Byte` (if we
had a `Byte` type), than like elm's `Bytes`.

    ByteArray.elm
    ByteArray/Builder.elm
    ByteArray/Parser.elm

    ByteArray/Compression.elm
    ByteArray/SevenBit.elm
    ByteArray/String.elm
    Windows1252.elm

### Add ons to standard system types

Many of these are similar to larger packages out there, but I only needed a bit
and I didn't think it worth adding the dependency.

    Html/Aria.elm
    Missing/Html/Events.elm
    Missing/List.elm
    Missing/Maybe.elm
    Missing/Regex.elm
    Missing/Time.elm

## The Elektron stack

### SysEx

Messages & wire format for the two kinds of SysEx that the instrument has.

    SysEx/ApiUtil.elm
    SysEx/Message.elm

    SysEx/Dump.elm

Managing sending and receiving

    SysEx/Client.elm
    SysEx/SysEx.elm

    SysEx.elm

Misc. stuff

    SysEx/Connect.elm
    SysEx/Debug.elm
    SysEx/Emulation.elm
    SysEx/Internal.elm


### Digitakt

Low-level project data structures, storing and decoding the binary storage
structures the instrument exchanges.  `StructUtil` is some heavy functional
programming magic that is pretty cool, if I do say so myself!

    Elektron/StructUtil.elm

    Elektron/Digitakt/Dump.elm

    Elektron/Digitakt/Blank.elm
    Elektron/Digitakt/BlankData.elm
    Elektron/Digitakt/CppStructs.elm

High-level project data structures, forming a logical view of the project
for the rest of the application.

    Elektron/Digitakt/Types.elm

    Elektron/Digitakt/HighLevel.elm
    Elektron/Digitakt/Related.elm
    Elektron/Digitakt/Verify.elm

+Drive structure, which works for other Elektron instruments as well.

    Elektron/Drive.elm
    Elektron/Path.elm

    Elektron/Digitakt/FactorySamples.elm

Miscellaneous

    Elektron/Instrument.elm

## The application

Note: The HTML is based on Bootstrap, but without any Elm wrapper library.
So, the views in these modules are all directly built from `Html` as needed
by Bootstrap. See, for example, `Alert` or `Progress`.

### Utilities

    Alert.elm
    Commands.elm
    Job.elm
    Progress.elm
    Undo.elm

### Sample tree

    Samples.elm
    Samples/Base.elm
    Samples/UpdateAction.elm
    Samples/Ops.elm

    Samples/Selection.elm
    Samples/Transfer.elm

    Samples/Update.elm
    Samples/View.elm

### Project

    Project.elm
    Project/Base.elm
    Project/Selection/Bank.elm
    Project/Selection/Project.elm
    Project/Update.elm
    Project/View.elm

    Project/Import.elm
    Project/Selection/Import.elm

    Project/Util.elm

### Main

    Main.elm
    Main/Base.elm
    Main/Control.elm
    Main/View.elm
    Main/MidiSetupView.elm
    Main/SettingsView.elm

### Other pages

    Help.elm
    Help/Contents.elm

    WebMidi.elm

## Miscellaneous cruft

    Build.elm
    Portage.elm
      -- see also portage.js over in the assets directory
    Report.elm
    Util.elm


## Other files

  * `assets/` - images, css, a font, bootstrap, jquery
  * `tests/` - a paltry set of unit tests
  * `*.html` - the top level pages
  * `*.sh` - shell scripts for building, see the Building section of README.md

# Coding Style

First rule of code style: **Don't use `elm-format`**

Seriously, I believe that laying out code carefully is a major aid to
communicating with other programmers. Things like `elm-format` impose a
one-size fits all approach, with no semantic understanding of the code. While
I like much of what elm-format does, some rules I don't agree with, and
sometimes there are important reasons to break otherwise good rules.

Happy to have conversations about this over beer... but not here.

I am a big fan of single letter variable names. Between the types and the
operations, the names of the variables often just clutter things up.

Don't bother putting comments on small, well named one or two line functions.

While this code has several libraries of low level functionality that could be
set up as independent packages, they aren't commented that way. In particular,
there aren't a lot of examples in the comments that exist. This is because
the application as a whole contains many examples of use of these functions.
Just use your IDE to find the references.

Try to import imported modules the same way: using the same alias, and exposing
the same things (usually just the main type):

    import ByteArray exposing (ByteArray)
    import Elektron.Digitakt.HighLevel as DT

Separate system imports from imports of modules within the app itself.

Editor settings:
  * 2 space indents
  * no tabs
  * no trailing whitespace
  * end with a newline or two

