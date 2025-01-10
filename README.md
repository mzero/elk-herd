# elk-herd

**elk-herd** is a device manager for some Elektron instruments:

### **+Drive Sample management** for Digitakt and Analog Rytm

* Transfer files to and from your computer with simple drag-n-drop
* Reorganize the +Drive with drag-n-drop
* Easily rename files and folders
* View your whole +Drive structure at once

### **Project & Pattern management** for Digitakt

* Transfer projects to/from your computer
* Reorganize patterns, the sample pool, and the sound pool with drag-n-drop
* All sounds and plocks are kept in sync with all changes
* Find unused sample and sound slots
* Move free space in the sample and sound pools to the end
* Import patterns, sample pool entries, and sounds from one project into another

### Videos

* DaveMech's awesome walk through:
https://youtu.be/P0NfvNJ6JM0
* Demonstration of project import:
https://youtu.be/VF_0y3_9uj0
* Short manual for other features:
https://youtu.be/j6yZ83FP6Qk

### Using it

**elk-herd** is hosted on the internet by the author for all to use. You need a
browser that supports WebMIDI - which means Chrome for now - and once the page
is loaded it runs locally on your machine:

> https://electric.kitchen/crunch/elk-herd/

If you prefer, you can download the archive, unpack it, and open it on Chrome
locally on a computer with no internet net access:

> https://electric.kitchen/crunch/elk-herd-live.tgz

Or you can build it and run it yourself...

### Known Issues

* Be aware that if **LFO Dest** is set to  **Sample Slot**  (in the track or
plock’d): If you rearrange the sample pool, you have to know which samples need
to be kept next to each other.
* There are still problems with using **elk-herd** on Linux due to known
Chromium browser bugs. I'm still trying to work out a work around.

You can report bugs [here](https://github.com/mzero/elk-herd/issues) on GitHub.


### **elk-herd** is free!
Enjoy, have fun, make music.
And if you like it, buy me a [beer](https://www.paypal.me/MtnViewMark)!

----

# Building it

**elk-herd** is written in [elm](https://elm-lang.org), a pure, functional
language for the web.

## 1. Download and install elm:

> https://guide.elm-lang.org/install/elm.html

## 2. Dependencies
There are a few external elm packages which will be downloaded the
first time you build. **elk-herd** also makes use of
  [jQuery](https://jquery.com/),
  [Bootstrap](https://getbootstrap.com/),
  and the font [Source Code Pro](https://fonts.google.com/specimen/Source+Code+Pro).
These can be downloaded and placed into the proper place in the tree by running
this script:

> `./get-ext.sh`

## 3. Build the development build

> `./make-dev.sh`

Then open `./index.html` in your browser.

## 4. Build the production build
If you want to build the production build, you need a javascript
minimizer like [terser](https://terser.org/). Install **terser**, if needed:

> `npm install terser -g`

and now

> `./make-prod.sh`

*If you want to use **uglifyjs** or **closure compiler** instead, or even no
minimizer, there is support in that shell script, but you need to edit it to
select it instead of **terser**.*

Now you have a tree at `distribution/elk-herd/` that you can serve
from a web server.  It's also already tar-ball'd up for you:
`distribution/elk-herd-live.tgz`

## 5. Explore the code
There is a code over view, and other coding notes in the **CONTRIBUTING.md**
document.

---

# Thanks
Thanks to all the people who tested the alpha and beta versions over the years!
You were very brave to trust your Digitakt projects to me, and I couldn't have
done this without you!

Thanks to my husband who puts up with me getting totally obsessed with a
coding project.
