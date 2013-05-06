# VimSpeak #

VimSpeak lets you control Vim with your voice using speech recognition. For instance, you can say _“select three words”_ to type `v3w` or _“change surrounding brackets to parens”_ to type `cs])` or crazy things like _“change occurrences of ‘foo’ into ‘bar’, globally, ignore case, confirm”_ to type `:%s/foo/bar/gic`. Of course in insert mode you may dictate whatever you like. To learn the grammar, have a look at the unit tests and the code (“use the source, Luke”). It’s quite declarative and easy to follow.

The idea is to run this console app in the background where it will listen for speech and do `SendKeys(...)` to the foreground app. The foreground app may be any editor expecting Vim keystrokes. It’s been tested with Visual Studio with [Jared Parson’s excellent VsVim extention](https://github.com/jaredpar/VsVim) (also written in F#, BTW) and with Sublime Text in Vintage Mode and, of course, with Vim itself.

---

It is written in F# and makes use `System.Speech.Recognition`/`Synthesis` (.NET 3+). It also relies on `SendKeys(...)` (.NET 1.1+). I’ve only tested it under Win8, but it may work with Mono on other platforms.

---

This is a little toy I built over the weekend after watching Tavis Rudd's nifty Strange Loop talk here: http://www.infoq.com/presentations/Programming-Voice
I wanted to do the same for Vim. It's nowhere near yet, but a start...

There's a demo/explanation of it here: http://www.youtube.com/watch?v=TEBMlXRjhZY

And another demo applying it to a VimGolf challenge: http://www.youtube.com/watch?v=qy84TYvXJbk

Have fun!
