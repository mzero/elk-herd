module Help.Contents exposing
  ( Article
  , Help(..)
  , help
  , helpStart
  , findArticle
  )

{-| The help content as a series of organized articles.
-}

import Html
import Markdown


type alias Article msg =
  { id : String
  , title : String
  , body : Html.Html msg
  }

idFromTitle : String -> String
idFromTitle =
  String.toLower
  >> String.words
  >> List.map (String.filter Char.isAlphaNum)
  >> String.join "-"

article : String -> String -> Article msg
article title text =
  { id = idFromTitle title
  , title = title
  , body = Markdown.toHtml [] text
  }


type Help msg
  = Entry (Article msg)
  | Section String (List (Help msg))

entry : String -> String -> Help msg
entry title body = Entry <| article title body


findArticle : String -> Maybe (Article msg)
findArticle id =
  let
    go hs =
      case hs of
        [] -> Nothing
        (h :: rest) ->
          case h of
            Entry a ->
              if a.id == id
                then Just a
                else go rest

            Section _ subHs ->
              case go subHs of
                Just a -> Just a
                Nothing -> go rest
  in
    go help




helpStart : Article msg
helpStart =
  article "Welcome" """

I wrote ```elk-herd``` because I wanted to be able to manage my Digitakt in ways
that existing software didn't do. I hope you find it useful too.

### First things first:

* This was created by me, [mzero](https://www.elektronauts.com/u/mzero/),
  not Elektron. They don't support it.

* If things go wonky, help me debug it - don't yell at me. This is just
  me in my spare time.

* Your stuff is private: Even though this is a web app, all your
  music, patterns, and samples never leave your computer.

**Enjoy. Have fun. Make music. Buy me a [beer](https://www.paypal.me/MtnViewMark)!**
"""

help : List (Help msg)
help =
  [ Entry helpStart

  , Section "Sample Management"
    [ entry "Get Started" """

* Hover over a directory to see the sample list.
* Click a directory to select it, and the sample list will stay put.
* Multi-select also works: Use shift key or command/windows key.
* Click the ```all``` button above the sample list area to show all the samples,
  in-line with the directories. This is useful for printing.

**Note:** Some operations take some time and you get a progress box. These
can be canceled, but that only stops the operation in progress. There is no
**undo**, so be careful.

If you manipulate the samples on the instrument directly, the sample list here
will get out of sync with what is on your machine. Use the **[Scan +Drive]**
button to rescan your drive.
"""

    , entry "Renaming" """

* Click a directory or sample to select it.
* Click **[Rename]** button, or press the return key.
* Type.
* Click either **[OK]** (or press return key), or **[×]** (or press esc key).

_Character Set:_ Elektron instruments use a particular character set(*).
If you try to enter a character they can't use, it'll be dropped or converted
to a `?`.

(*): [Windows-1252](https://en.wikipedia.org/wiki/Windows-1252)
"""

    , entry "Moving" """

* Select one or more items.
* Then either:
  * Drag them onto a directory, and drop them.
  * Click **[Move]** button. then click on the destination directory.

If you move a directory tree, the whole tree will be created where you dropped
it, the samples moved, and the original tree removed.

When you move samples around, they will stay loaded in any projects.
"""

    , entry "New Directory" """

* Click **[New Directory]** to create a new directory in the selected directory.

The directory is immediately started for rename, so can just type the name you
want and hit return.
"""

    , entry "Deleting Files" """

* Click **[Move to Trash]**, or press the `delete` key with something selected.

Rather than deleting things directly, this manager creates a folder at the
top level called `trash`. The **[Move to Trash]** button will then move the
selected Sample or Directory to the trash.

The **[Empty Trash]** command does what you think, and takes care of deleting
samples first, then the directories once empty.
"""

    , entry "Transferring Samples" """

### To transfer to the device:

* Click **[Send Sample File]**, pick a file from the file picker.
* _or_ Drag files from your desktop onto a directory in the +Drive. You can
drag a folder as well, to send a whole tree.

### To transfer from the device:

* Select a directory in the +Drive, then select one or more sample files.
* Click **[Get Sample File]**.

If you select multiple files to get at once, the browser will ask you if you
want to allow the app to send multiple downloads the first time.

For now, you must select the sample files explicitly, you can't just select
a +Drive directory, and download a tree of samples.
"""
    ]

  , Section "Digitakt Projects"

    [ entry "About Projects" """

If you have a Digitakt, then you'll see an extra option above the commands
on the left: **[Projects]**. Clicking that will take you the project management
section of ```elk-herd```.

### Projects are like documents

Unlike managing samples, projects are not altered on your instrument while you
edit them in ```elk-herd```. Instead, they are like documents: You can open one,
edit it, but until you save or send it - nothing anywhere is changed.

### Projects and +Drive Samples

While projects use samples, the samples aren't part of the project. Instead,
the project contains a reference to the sample on the +Drive. This is how the
Digitakt works.

When ```elk-herd``` has a project loaded, it uses the +Drive of your connected
instrument to find the names of the samples used in the project. If you load
up an old project that references samples that are no longer on your +Drive,
they'll show up as ```???```. That's okay, you can still work with them, and
if you later transfer the samples back to the +Drive, they'll work as expected.
"""

    , entry "Managing Projects" """

### With the Digitakt

Projects can be transferred to and from your instrument. The commands on the
left are:

**[Fetch Project]** reads the currently loaded project on the Digitakt
into ```elk-herd```. If you've modified the project but haven't saved it to
the +Drive yet, you'll be loading the modified version.

**[Send Project]** sends the project in ```elk-herd``` to the Digitakt,
overwriting the currently loaded project. This can't be undone, so be careful.

* You'll see a progress bar both on the computer and on the Digitakt. The
computer will finish first. Wait until the Digitakt is done before doing
anything else.

* It is good practice to save the project on the
Digitakt to the +Drive with **System** > **Project** commands.

* _Careful:_ Using the **Save Proj** short cut on the Digitakt will overwrite
whatever project you last loaded.

### With Files

Projects can be loaded and saved to ```.syx``` files. These are MIDI System
Exclusive files and are in exactly the same format as transferred by the
Digitakt using the **System** > **Sysex Dump** menu.

**[Open File]** lets you pick a previously saved file and load it into
```elk-herd```

**[Save File]** writes the current project as a file. You'll see it in
the downloads of your browser.
"""

    , entry "Project Items" """

Digitakt projects consist of three banks of items: Patterns, the Sample Pool,
and the Sound Pool. These are shown as grids.

Hovering the mouse over any item will highlight the other items that are related.
For example, hovering over a pattern will show you which sample slots it uses.

### Selection

You can select items using the usual mouse and keyboard gestures:

* Click on an item to select just it.
* Click on an empty space to clear the selection. (This will return to the
hover behavior).
* Shift-Click will extend the selection.
* Cmd-Click will let you add (or remove) a non-adjacent item.
* You can only select one type of item at a time.

When selected, the other banks of items show you the items the selection uses
or is used by. For example, selecting some samples shows you the patterns that
use them.

### Item Commands

There are some specialized commands on the right of each bank's heading:

* **[\u{21E2}]** Makes the related items the selection.
* **[\u{2205}]** Selects the samples or sounds that aren't used by any pattern
or sound.
* **[\u{270E}]** Allows you to rename the selected patterns or sounds.
* **[\u{2AE9}]** Compacts the sample or sound pool so all free space is at the
end.
* **[\u{1D2C}z]** Sorts items by their name
* **[\u{222E}]** Sorts samples by their path in the +Drive, which will group
them logically.
* **[\u{00D7}]** Deletes the selected items, but only if they are not used by
any pattern or sound.

### Digitakt II Sample Pool

The Digitakt II has a very large sample pool, divided into 8 banks,
labeled A though H. The bank selector in the center of the Sample Pool area
shows you which bank is being displayed. The selector also shows empty banks
in grey, and banks with selected or related items with a bar over the letter.

Note: Compact only works on the current bank.  Sorting, on the other hand
sorts the whole pool.
"""

    , entry "Rearranging Items" """

Items can be rearranged in their banks simply by selecting, and then dragging
them around. As you drag, you'll see an indication of where they will land.

As you drag, other items will "move out of the way" so that you can insert a
group of items in front of others.

You can rearrange samples and sounds, and all pattern tracks and plocks will
be updated with the changes so that everything still plays the same.

### Digitakt II Sample Pool

To move items from one bank to another, drag them up to the bank selector of
the bank you want, and then straight down into the bank.

----

_Exception:_ If you have the **LFO** **DEST** set to control
**SAMP:Sample Slot**, the sample slots around the track's sample can be
played, even though the pattern doesn't reference them explicitly. If you do
this, then you need to be sure to move those samples around as a block.
"""

    , entry "Renaming Items" """

Patterns and Sound pool items can be renamed. Select one or more, and
then use the **[\u{270E}]** button, or just hit the **[Enter]** key.

**[Tab]** will move you from one item to the next, so you can quickly rename
several items.

Use the **[\u{270E}]** button, or just hit the **[Enter]** key again to exit
renaming.

### Character Set

Pattern and Sound names can use only a limited set of characters:

            A B C D E F G H I J K L M N O
            P Q R S T U V W X Y Z Å Ä Ö Ü
            ß Æ Ø Ç Ñ 0 1 2 3 4 5 6 7 8 9
            ~ ! @ # $ % ^ & ( ) _ + - =

"""

    , entry "Importing from Projects" """

_Finally! **THIS** is why I wrote ```elk-herd``` in the first place!_

You can import selected items (patterns, sample pool entries, sound pool entries)
from one project into another. This allows you to finally assemble projects
for all the pieces you have scattered all over your Digitakt!

### Importing is a multi-step process:

1) Either start with an empty project, or a project you want to add into.
  * **[Clear Project]** command for an empty project
  * or **[Fetch Project]** or **[Open File]** to start with an existing project.

2) Use the **[Import File]** or **[Import Project]** command to select the
project you want to copy _from_.

3) A new set of pattern, sample, and sound banks appears with a red import
header above them.

4) Select the items to import.

  * Selection mostly works as you expect, but here clicking simply toggles
  items on or off, and dragging just extends the selection.

  * You don't need to selected related items, they'll be imported automatically.
  For example, if you select a pattern, all the samples and sounds it needs
  are highlighted for import as well.

  * The section headers above each bank give you a summary of what will happen.

  * You _can_ select different types of items at the same time for import.

5) Click **[Import]** in the red Import bar at the top.

  * Items will be copied into free slots in the receiving project.
  * Items which were already in the receiving project won't be duplicated,
  and patterns will be fixed up to use them.

You can now import from another project if you want. If everything is as you'd
like it, remember to save the project to a file or send it to the Digitakt.

----

_Note:_ If you import patterns that make use of samples that are not loaded
in your +Drive (they show up as ```???```), the import will still work and do
what you want. However, before those patterns will play correctly, you'll need
to reload those sample files to the +Drive.  (Which you can do with ```elk-herd```,
natch'!)
"""
      , entry "About Phantoms" """

Occasionally you'll see items in a project that are drawn in grey. These are
items that the Digitakt considers "empty", but ```elk-herd``` has determined
otherwise. We call these "Phantoms":

* Patterns with no trigs are considered empty by the Digitakt. However, if the
pattern has a name, or any of the track source samples are other than the
default, or any of the MIDI tracks have been enabled, then these patterns are
considered phantoms.

* Empty sample pool or slots that are nonetheless referenced by pattern tracks or
plocks, or sound pool items are phantoms. These will be named ```---```

* Empty sound pool slots that are referenced by pattern sound plots are phantoms.
These will be named ```--```

Phantoms operate no differently than other items in ```elk-herd```, except that
on import, they will not be merged with other phantoms, even though they are all
"empty".
"""
    ]

  , entry "FAQ" """

### I found a bug... Where do I report it?

If you use GitHub: [mzero/elk-herd](https://github.com/mzero/elk-herd/issues)

Or on the [Elektronauts forum
](https://www.elektronauts.com/latest). Either message me
([@mzero](https://www.elektronauts.com/u/mzero)), or post to one of the
[elk-herd topics](https://www.elektronauts.com/search?expanded=true&q=elk-herd%20in%3Atitle)


### What happens if I move a sample I've used a sample in a project?

It stays loaded in the project. Projects reference samples by unique
_hash codes_ (like fingerprints). You can move a sample around the +Drive,
or even rename it, and projects and sounds will still use it.

If you accidentally delete a sample you are using in a project, transferring
the same original sample file will result in the same hash code, and it
will "hook up" again to the project! **Note:** this may not work on samples that
need to be sample rate converted.

### Will this work for my other Elektron instruments?

Sample management should work for Analog Rytm & Model:Samples.

Project management is only implemented for Digitakt & Digitakt II.

### Why can't I do anything with the `/factory` content?

Elektron has made it locked. But, they say it doesn't take any space from the
1GB available to you on the +Drive. I'm not sure if that means that it is
stored somewhere else, and really isn't there, or that the +Drive has more
enough spare space to store 1GB of your samples plus the factory
content.

### Why isn't the `/factory` content shown?

Elektron machines don't report the `/factory` directory to external software.
Don't fret, it's all there, we just can't show you the tree.  Older device
OS versions used to show it... newer ones might again one day...

### Is this project Open Source?

YES! It's on github:
[mzero/elk-herd](https://github.com/mzero/elk-herd)

"""

  , entry "Credits" """

I created this, because I wanted to manage my Digitakt. Elektron and
others create software for managing the Digitakt, too. Let a thousand
flowers bloom, I say!

This is hosted on my own web site and free to use.

This app is written in [Elm](http://elm-lang.org/), by me, Mark Lentczner.
You can find me Elektronauts as [mzero](https://www.elektronauts.com/u/mzero/),
or if you prefer, [mark@glyphic.com](mailto:mark@glyphic.com).

I'd like to acknowledge the Elektron technical team, who has fielded my
arcane technical questions over the years, and provided me with technical
details.

Elk image credit: [Airman 1st Class Kaylee Dubois
](https://www.jble.af.mil/News/Photos/igphoto/2001755026/)
"""
  ]

