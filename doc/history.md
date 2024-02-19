# ASDF History

This document presents the history of ASDF, its past, present and future,
and those of its known alternatives.

See also the many documents listed on the
[asdf homepage](https://asdf.common-lisp.dev/#documentation)
about ASDF, especially the ILC papers from
[2010](https://gitlab.common-lisp.net/asdf/ilc2010),
[2014](https://github.com/fare/asdf3-2013) (the most detailed) and
[2017](https://github.com/fare/asdf-2017)
for more detailed information about the design of ASDF and its evolution.

## Chronology of ASDF

Here is the chronology of ASDF releases, from newest to oldest.
For a detailed description of recent changes see our [Changelog](changelog).
For even more details, look at the git history.

### January 2024: ASDF 3.3.7

Seventh bugfix release for the 3.3 release series,
it notably adds compatibility with Allegro CL 11.0.

### August 2022: ASDF 3.3.6

Sixth bugfix release for the 3.3 release series.

### July 2021: ASDF 3.3.5

Fifth bugfix release for the 3.3 release series.

### February 2020: ASDF 3.3.4

Fourth bugfix release for the 3.3 release series.

### March 2019: ASDF 3.3.3

Third bugfix release for the 3.3 release series.

### May 2018: ASDF 3.3.2

Second bugfix release for the 3.3 release series.

### November 2017: ASDF 3.3.1

Bugfix release for 3.3.0 addressing a backwards-incompatibility issue
with timestamp computations, and other minor breakage.

### October 2017: ASDF 3.3.0

This release introduces a substantial revision to the build plan
to correctly handle multiple phases of loading, due to e.g.
`DEFSYSTEM-DEPENDS-ON` dependencies.

### April 2017: ASDF 3.2.1

Release with many small bugfixes and cleanups.
Presentation at ELS 2017 about ASDF 3.3.

### January 2017: ASDF 3.2.0

A release containing many notable improvements,
such as a new portable `uiop:launch-program` facility
for spawning asynchronous subprocesses (with many thanks to Elias Pipping),
a `uiop:with-deprecation` facility to handle progressive deprecation of functions,
a cleanup and tightening of the internal dependency model
(you are now required to use `make-operation` to instantiate an operation class;
also you'll be WARNed if your `.asd` file contains
improperly named secondary systems),
a systematic pass of adding documentation to all functions,
many fixes to small bugs and portability issues
across all underlying platforms and operating systems,
an improved test suite,
and the removal of some long deprecated functionality.

### March 2016: ASDF 3.1.7

Another bug fix release for the 3.1.x series.

Note that ECL forked ASDF and issued a 3.1.8 rather than adopt ASDF 3.2.x,
but that is not an official release by the ASDF team.

### October 2015: ASDF 3.1.6

Although we had hoped that ASDF 3.1.5 would be the final release
in the ASDF 3.1 series, a number of bug reports led us to prepare release 3.1.6.
Support for Windows continues to improve, and we wished
to release a number of bug fixes, and support
the recent Allegro Common Lisp 10.0 release.

### July 2015: ASDF 3.1.5

An extensive bout of bug-fixing, notably on Windows, leads to
release of ASDF 3.1.5 on 21 July 2015.
XDG handling has been improved to be more compliant with the standard.
*Preliminary* support for immutable systems has been added.

### May 2015: ASDF 3.0 is now universally supported, ASDF 2.0 unsupported

With the LispWorks 7.0 release, all actively maintained CL implementations
are now providing ASDF 3.0 or later, and
support for older variants is now officially dropped.

### October 2014: ASDF 3.1.4

More bug fixing leads to release of 3.1.4 on 10 October 2014.
There should be no incompatibilities.

### August 2014: mailing-list update

The ASDF mailing lists have been reestablished, in particular `asdf-announce`,
which should allow CL implementers better access
to only the information they want about ASDF development.

### May 2014 to July 2014: ASDF 3.1.3

ASDF bug fixing from 3.1.2 leads to release of 3.1.3, a major bug fix release.
We strongly urge implementors that have shipped with 3.1.2 to upgrade to 3.1.3.
There should be no incompatibilities, and
some very important bug fixes are provided.

### July 2013 to May 2014: ASDF 3.1

François-René Rideau has resigned as maintainer but remained an active developer.
Robert P. Goldman is interim maintainer until someone more gifted,
charming, dedicated, and better-looking can be secured to fill the role.
ASDF 3.0.2 was released in July 2013, 3.0.3 in October 2013, and 3.1.2 in May 2014.
In addition to significant improvements and bug fixes,
notably better Windows support,
ASDF 3.1.2 notably sports the `package-inferred-system` extension.

### November 2012 to June 2013: ASDF 3.0

François-René Rideau completely rewrites ASDF and publishes ASDF 3,
pre-released as 2.27 in February 2013, and released as 3.0.1 in May 2013.
It now includes both the traditional `asdf/defsystem`
and a formalized portability library `uiop` (née `asdf/driver`).
`asdf/defsystem` is a backward-compatible reimplementation of ASDF
with correct timestamp propagation based on a consistent dependency model,
and featuring support for bundle output, deferred warnings check, and more.
`uiop` provides many abstractions to write portable Common Lisp programs.

Last version: 3.0.1.

### December 2009 to October 2012: ASDF 2

François-René Rideau is de facto maintainer,
with notable contributions from Robert P. Goldman, but also
Juanjo Garcia-Ripoll and James Anderson.

Many usability and portability concerns are addressed
ASDF 2.000 is released in May 2010
with many clean-ups, better configurability, some new features,
and updated documentation.
The ASDF 2 series culminates with ASDF 2.26 in October 2012,
which in addition to many bug fixes and small features
includes support for file encodings, around-compile and compile-check hooks.

Last version: 2.26.

### May 2006 to November 2009: ASDF 1.xxx

Gary King is de facto maintainer, with notable contributions from
Robert P. Goldman, Nikodemus Siivola, Christophe Rhodes, Daniel Herring.

Gary takes ASDF from a hobby project to professional quality software,
making it maintainable, moving to using git and common-lisp.net,
and adding many small features and bug fixes.

Last version: 1.369.

### May 2004 to April 2006: Minimal Maintenance of ASDF 1.xx

Christophe Rhodes is de facto maintainer,
with notable contributions from
Nikodemus Siivola, Peter Van Eynde, Edi Weitz, Kevin Rosenberg.

The system made slightly more robust, a few more features.
By now SBCL has become the defacto standard build system
for new free software CL packages, but has many
annoying bugs, weird corner cases and non-portable behaviors.

Last version: 1.97.

### August 2001 to May 2004: Daniel Barlow's original ASDF

ASDF is created then developed by Daniel Barlow,
with notable contributions from
Christophe Rhodes, Kevin Rosenberg, Edi Weitz, Rahul Jain.

The system quickly attracts a small community of users,
as a much more usable alternative to mk-defsystem.
Dan Barlow himself writes systems not just for all his software,
but for many libraries that he uses.
The system is primarily developer on and for SBCL, but soon enough
also supports Allegro, clisp, CMUCL, CormanLisp, ECL, Genera, LispWorks, MCL, OpenMCL, SCL.

See this first [public announcement of ASDF](https://sourceforge.net/p/cclan/mailman/message/2011712/)
in late July 2001, even before it was first working and committed to CVS!
(It was since then migrated to git). The proposal page linked is long dead, but
should more or less match the README in the earliest versions of ASDF in git.
The initial revision of ASDF was only 418 lines long,
compared to 14131 for ASDF 3.3.7.1 in 2024.

Last version: 1.85.

## Former ASDF Extensions, Now Superseded

Below is a list of former extensions to ASDF, now superseded, with a reason why.
See [our webpage](index.html#extensions) for currently active extensions of ASDF.

### asdf-binary-locations

ABL used to allow one to redirect where ASDF 1 created its output files,
so they don't clash between implementations
and don't pollute source directories.
Since ASDF 2, it is superseded by
`asdf`'s builtin `asdf-output-translations` mechanism;
a limited compatibility mode is available to easily convert
your former ABL configuration into an AOT configuration.

`common-lisp-controller` and `cl-launch` used to provide similar mechanisms,
and have also been superseded by `asdf-output-translations`

### asdf-bundle

`asdf-bundle`, née `asdf-ecl`, allowed you to create
a single-file bundle out of a system, for easier delivery.

Since ASDF 3, it is now a builtin part of `asdf/defsystem`,
and allows users to deliver a single FASL for a system,
a standalone executable program (on supported implementations),
or an image containing your system precompiled.

### asdf-condition-control

Initially part of XCVB's `xcvb-driver`,
`asdf-condition-control` allowed you to muffle
uninteresting conditions during compilation.

Since ASDF 3, it is now superseded by equivalent functionality in `uiop`.

### asdf-contrib

[asdf-contrib](https://gitlab.common-lisp.net/asdf/asdf-contrib)
is an empty package that used to collect dependencies on other ASDF extensions,
some current and obsolete. It has been unmaintained since before ASDF 3.

### asdf-package-system

[asdf-package-system](http://gitlab.common-lisp.net/asdf/asdf-package-system)
extended ASDF 3.0 to enable compilation of Lisp source files in a one-package-per-file style
wherein the file's topmost `defpackage` also determines dependencies.
This style was inspired by `quick-build` and `faslpath`
(see discussion of them below in the section "Alternate Lisp Build Systems").

Since ASDF 3.1, this functionality is built into ASDF as
`package-inferred-system`. See the manual for documentation.

### asdf-utils

`asdf-utils` was a collection of utilities that originated with ASDF.

Since ASDF 3, it is now superseded by `uiop` (a.k.a. `asdf/driver`),
which is part of ASDF, and exports its functionality in its own package `uiop`.

## Alternate Lisp Build Systems

There are or have been many build systems for (Common) Lisp software,
many of which have informed the design ASDF, positively or sometimes negatively,
and a few that have been informed by ASDF, also one way or the other.
Here are those we are aware of. There are probably more.

However, note that none of these systems seems to ever have reached
a fraction of the traction of ASDF, probably because none was technically
superior and/or portable enough (if at all)
to compensate for the first mover advantage.

### Bazel

Google's deterministic and scalable build system [Bazel](https://bazel.build/) (2015-)
has support for Common Lisp: [bazelisp](https://github.com/qitab/bazelisp) (2016-),
notably used by Google Flights (née QPX at ITA Software).

Given a farm of worker machines, it can massively parallelize the
deterministic compilation of Lisp individual files by SBCL,
each compiled "hermetically" in its own process
after loading all its individual dependencies with the SBCL interpreter,
which is very fast for this purpose.

### quick-build

Alastair Bridgewater's small and simple one-package-per-file
[quick-build](https://bugs.launchpad.net/asdf/+bug/1230368) (circa 2012-2013)
is a complete though barebones build system in about a hundred lines of code.

Its design notably inspired `package-inferred-system`,
a feature present in ASDF since version 3.1 (2014).
The ASDF variant of the idea is much bigger, especially if you consider that
it imports all of ASDF and UIOP;
but even then the size is still quite small by modern standards,
and you can use all of ASDF (which you would have to do anyway
unless you had no dependency to any library).
See also faslpath below for another system of similar design.

### XCVB

François-René Rideau's [XCVB](https://common-lisp.net/project/xcvb/) (2008-2012)
was meant to build objects and image files deterministically and in parallel.

It once worked, but introduced significant migration costs to move from ASDF
(though it also supported some level of interoperation with ASDF),
and never received wide adoption.
The author stopped actively developing it after his employer ITA
lost interest in deploying it, and it bitrot for good
after the author became maintainer of ASDF itself instead.

Many ideas from XCVB made their way into ASDF 2's the configuration system,
and ASDF 3's compatibility layer UIOP (initially known as asdf-driver,
that imported a lot of code from xcvb-driver, the portability layer of XCVB).
Lessons from XCVB's failures also informed what <em>not</em> to do in ASDF:
XCVB tried to introduce a new unified namespace for all source files and
build artefacts potentially supporting all languages, which introduced
too much complexity and incompatibility for insufficient gains.
By contrast, the changes made to ASDF 2 and 3 tried to be as simple,
unobtrusive and backward-compatible as possible.

The XCVB experience also influenced the latter Lisp support
for Bazel (see above).
Finally, leftover ideas from XCVB made their way to the
ASDF [TODO](https://gitlab.common-lisp.net/asdf/asdf/blob/master/TODO) list,
meant to inspire a future ASDF that properly supports cross-compilation
and maybe arbitrary build rules for many programming languages.

### MK-Defsystem

Mark Kantrowitz's [mk-defsystem](http://www.cliki.net/mk-defsystem) (1991-2005),
was the first free software successor of the old proprietary DEFSYSTEM's of old,
portable to a lot of Common Lisp systems from the early 1990s,
most of them deceased by the time ASDF appeared in 2001.
Many of these Lisp systems didn't support CLOS at all, or not well,
and mk-defsystem didn't use it, making it hard to extend.
mk-defsystem was also hard to configure,
usually involving a system administrator setting up "logical pathnames" (LPNs).

ASDF was developed in notable reaction to mk-defsystem,
as a new simpler system that was extensible,
based on the OO design once suggested by Kent Pitman,
and using pathnames in a way that "played nice with Unix".

Mark stopped maintaining it around 1995 or so,
and in the 2000s it was maintained by Marco Antoniotti.
The code was a mess, and ASDF disrupted the failed attempts
to modernize mk-defsystem.
See the defsystem-3.x and defsystem-4 projects in clocc,
the Common Lisp Open Code Collection.

### ASDlite

Dmitriy Ivanov's [ASDlite](http://lisp.ystok.ru/asdlite/) (2009-2015)
was a somewhat improved incompatible variant of ASDF 1.
But it is much less featureful, robust or portable than ASDF 2 or ASDF 3.

It seemed wrongheaded to me (FRR), but I suppose
the author probably had at least one good idea in it somewhere.

### YTools

Drew McDermott's [YTools](http://cs-www.cs.yale.edu/homes/dvm/) (1976-2009)
was the polar opposite of XCVB:
its YTFM (YTools File Manager) tried to maintain coherence
of the current Lisp image at a fine grain, down to "segments" of a file.

Sadly, Drew recently passed away and no one took over his work.

### mudballs

Sean Ross's [mudballs](http://sean-ross.blogspot.com/search/label/mudballs") (2008-2009)
was an aborted attempt at a slightly cleaner build system than ASDF 2.
There again, the gains were too small to justify the vast migration costs.

### faslpath

Peter von Etter's [faslpath](http://www.cliki.net/faslpath) (2009)
was a much simpler system than ASDF,
establishing a mapping between packages and files.
The idea was reinvented in an even simpler way by quick-build (see above),
and afterwards reprised by ASDF 3.1's package-inferred-system feature.

### evol

Alexander Kahl's [evol](http://www.cliki.net/evol) (2009-2010)
was a reimplementation in Lisp of the GNU autotools stack.
There again it was abandoned because it was too much work with too little gain
to justify massive transition costs.
