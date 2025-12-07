# Project Schemacs

#### A clone of Emacs and Emacs Lisp written in R7RS Scheme

## Project Goals

Although this project is still incomplete and experimental, the goals of this project are to construct a Scheme app platform similar to Emacs, not just in the UI/UX, but also to be backward compatible (to the greatest degree possible) with GNU Emacs by implementing an Emacs Lisp interpreter as well.

- written in [**portable**](../../../wiki/Portability.md) R7RS Scheme, should run on any compliant Scheme implementation.
- able to run your `init.el`, run Emacs software pulled from ELPA.
- use Emacs Regression Tests (ERT) from GNU Emacs to ensure compatibility.
- encourage the use of the Scheme programming language to develop apps and text editing workflows.

### Sub-goals

- contribute patches upstream to the Guile Scheme Emacs Lisp compiler
- provide a cross-platform GUI library like [Racket/GUI](https://docs.racket-lang.org/gui/) or [McCLIM](https://mcclim.common-lisp.dev/)
- be able to develop Schemacs from within it's own editor, create pull requests in Git.

## The Wiki

The wiki documents several of the important libraries in this source
code.

- [`(schemacs pretty)`](../../../wiki/PrettyPrinter.md): a pretty printer
- [`(schemacs elisp-eval environment)`](../../../wiki/ElispEnvironments.md): the environments in which Emacs Lisp code evaluates

## How to help contribute code

### Code of Conduct

We respectfully ask all contributors to this project adhere to the
principles stated in the [Code of Conduct
(CoC)](../../../wiki/CodeOfConduct.md), so please be sure you read and
understand this agreement before asking to contribute. The CoC is
currently based on the [Contributor Covenant 3.0 Code of
Conduct](https://www.contributor-covenant.org/version/3/0/code_of_conduct/).

### First steps toward contributing to Schemacs

There are many tasks to complete, many of which are fairly simple to
do even for people with not a lot of experience using the Scheme
programming language, as long as you can get the code in this project
to run on your Scheme implementation.  See the
["How to build"](#how-to-build), ["How to run"](#how-to-run), and
["How to hack"](#how-to-hack) sections below.

As of right now, the most important long-term issue to work on is
[#30 "*Get Emacs Regression Tests (ERT) to evaluate on the `cl-lib.el`*"](https://codeberg.org/ramin_hal9001/schemacs/issues/30).
Please refer to that issue for details, but briefly
here is what must be done: first try running the `elisp-tests.scm`
test program in your Scheme REPL...

```
(load "./elisp-tests.scm")
```

...if an error occurs, it will print the Emacs Lisp form that caused
the error. Since the test programs run by `elisp-tests.scm` all work
perfectly well in GNU Emacs Lisp, any error in Schemacs while evaluating
these tests is an implementation bug that must be corrected.

If you find a bug, check if has already been recorded in the
[list of open issues](https://codeberg.org/ramin_hal9001/schemacs/issues),
and if anyone is already assigned to working on it. If it is unassigned,
please comment on the issue that you would like to begin working on a
patch! You may also open a new issue if the bug seems not to have been
recorded yet, but please wait until you receive a reply from the
maintainers on your issue before starting work on a patch to make sure
you don't end up duplicating the work of others.

If you want to contribute, but are not interested in working on the
Emacs Lisp interpreter part of the Schemacs project, please feel free to
contact the author (contact details on
[Codeberg.org](https://codeberg.org/ramin_hal9001)) after reading
through the [list of open issues](https://codeberg.org/ramin_hal9001/schemacs/issues),
or reading over the [**feature wish list**](../../../wiki/WishList.md) document.

## How to build

As of right now, this project only runs on Guile Scheme, although
certain libraries (`lens.sld`, `pretty.sld`, `keymap.sld`) can build
and run on other Schemes. The only GUI available right now is for
[Guile-GI](https://github.com/spk121/guile-gi), but the Editor is
designed specifically to be able to run on other Scheme platforms with
other GUI toolkits. All platform specific calls are parameterized.

### Scheme Requirements

- [Guile 3](https://www.gnu.org/software/guile)

- [Guile-GI](https://github.com/spk121/guile-gi) must be built and
  installed in a directory path that is listed in the `%load-path`.

### C Requirements for Guile-GI

- `libglib`
- `libgio`
- `libgdk`
- `libgtk3`

[Guile 3](https://www.gnu.org/software/guile) usually installs from
source using `autotools` on any Linux or BSD operating system provided
the above developer dependency packages are installed. Installing with
`autotools` installs all Guile modules in the site-local package
directory. If you install it this way, the modules are always
available to your Guile runtime without needing to set the Guile
`%load-path`.

### (Optional) use Guix

The `manifest_guile-gi.scm` file is provided to install development
dependencies into your local Guix installation. Use it in one of two
ways.

 1. The first way is to use the `guix shell` command, which keeps
    package dependencies locally and temporarily (until you run the
    Guix garbage collector). The full command is this:

    ```sh
      guix shell -m ./manifest_guile-gi.scm -- guile --r7rs ;
    ```

 2. The second way is to installing packages in a local profile
    directory.  Python programmers may find this workflow more
    familiar, as it is similar to using a Python `venv` or "virtual
    environment", except the package directory is `./.guix-profile`
    rather than `.venv`.

    ```sh
      guix package -p ./.guix-profile -m ./manifest_guile-gi.scm -i ;
      guix shell -p ./.guix-profile -- guile --r7rs -L "${PWD}" ;
    ```

    And then, when you see the Guile REPL prompt `scheme@(guile-user)>`
    you can [run the Scheme programs](#how-to-run).

    Furthermore, the `guix shell` command can use the profile
    directory, and the package dependencies installed into the profile
    directory will survive Guix garbage collections.

### Launch the Guile REPL using `./guile.sh`

The `guile.sh` script sets environment variables and command line
parameters for the Guile runtime, it is usually easier to simply
execute this script to start the REPL.

Emacs users can set the directory-local variable `geiser-guile-binary`
to `"./guile.sh"`. The `.dir-locals.el` file in this repository does
this for you if you choose to use it.

#### (optional) Launch `guile.sh` in a Guix Shell

If you are using Guix Shell according to the steps in the section ["(Optional) Use Guix"](#optional-use-guix), you can run the `guile.sh` script like so:

```sh
guix shell -p ./.guix-profile -- sh ./guile.sh
```

## How to run

Once you have a Guile Scheme REPL running and you can see the
`scheme@(guile-user)>` prompt, and you are sure the Guile-GI
dependencies available in the `%load-path`, simply load the main
program into the REPL:

```scheme
(load "./main-guile.scm")
```

This will rebuild and launch the executable. If you are using the
`./guile.sh` script to start the REPL, note that the
`--fresh-auto-compile` flag is set, and so recompilation will occur
every time `main-guile.scm` is loaded. If you are not hacking the
Schemacs source code, feel free to delete this flag from the `guile.sh`
script file so that `load` only builds the application once, and
launches the application more quickly.

### Double-check the Guile `%load-path`

If you evaluate `,pp %load-path` in the `scheme@(guile-user)>` REPL,
the load path should look something like this:

```
scheme@(guile-user)> ,pp %load-path
$1 = ("/home/user/work-src/schemacs"
 "/usr/share/guile/3.0"
 "/usr/share/guile/site/3.0"
 "/usr/share/guile/site"
 "/usr/share/guile")
```

### Double-check the Guile `%load-path` in a Guix Shell

If you evaluate `,pp %load-path` in the `scheme@(guile-user)>` REPL
that was launched within a Guix Shell, the load path should look
similar to this, although likely with different hash codes in the
`/gnu/store`:

```
scheme@(guile-user)> ,pp %load-path
$1 = ("/home/user/work-src/schemacs"
 "/gnu/store/ylbycmajc0sf1pndfnsfql76cr1097iq-profile/share/guile/site/3.0"
 "/gnu/store/jqrkacxgsaf7b19xqzc2x4d77v27dbc6-guile-3.0.8/share/guile/3.0"
 "/gnu/store/jqrkacxgsaf7b19xqzc2x4d77v27dbc6-guile-3.0.8/share/guile/site/3.0"
 "/gnu/store/jqrkacxgsaf7b19xqzc2x4d77v27dbc6-guile-3.0.8/share/guile/site"
 "/gnu/store/jqrkacxgsaf7b19xqzc2x4d77v27dbc6-guile-3.0.8/share/guile")
```

## How to hack

Start a Scheme REPL as described above, but instead of running the
main program or the `./elisp-tests.scm` program, instead run the test
suite:

```scheme
(load "./run-tests.scm")
```

Unlike the `./elisp-tests.scm` program, the `./run-tests.scm` ensures
the exported symbols in each library all behave as expected. Before
you change anything, make sure you keep a log of which tests have
passed and which have failed, if any.

Whenever you make a change to the source code of a Scheme library, be
sure to run the tests for that library. The tests in `./run-tests.scm`
require your Scheme implementation to provide
[SRFI-64](https://srfi.schemers.org/srfi-64/srfi-64.html), without
this language extension (sorry, MIT Scheme users) you will have to run
each test case by hand by copy-pasting each test form into your REPL.

Keep in mind that this project is still experimental, it is possible
some of the tests may not pass. As long as you create a pull request
with equal or fewer passing tests, your request is more likely to be
pulled into the main branch.

That said, `./schemacs/lens.scm` and `./schemacs/keymap.scm` should always
pass all tests, as these libraries are most essential to the rest of
the application.

### Lenses

R7RS Scheme does not standardize any Meta-Object Protocol (MOP)
implementation, not even in the R7RS "Large" standard.

Schemacs has been written such that there no need for any MOP
implementation. Rather, a "function lenses" implementation written in
pure R7RS-Small compliant code is provided as a library. Functional
lenses are inspired by Haskell, and are a way of defining getter and
setter functions that can be composed together.

Conventionally, lens definitions are prefixed with the `=>`
symbol. They may also be suffixed with `*!` or `!` whether a lens is
canonical and/or whether a lens mutates the data structure when
updating it. For example, the `(schemacs editor)` library exports
symbols such as `=>editor-buffer-table*!` or `=>buffer-local-keymap*!`.

Since it is usually much easier to use lenses rather than getters and
setters, many Schemacs libraries export their own lenses for working
with the record types provided within.

Lenses are useful enough to be separated into it's own separate source
code package. See the documentation for Functional Lenses in its own
[source code repository](https://codeberg.org/ramin_hal9001/schemacs-lens).


### Platform-independent record types

Record types that contain platform specific information are usually
exported as lenses, and are usually named `=>*-view`, for example
`=>buffer-view` and `=>window-view`. These are fields that contain
references to platform-specific data needed by the platform in order
to render the view of a "buffer" or "window."

## Other resources

- [Presentation of this project at EmacsConf 2024](https://emacsconf.org/2024/talks/schemacs/)

## Contributors welcome!

This is a large and ambitious project, but there seems to be a lot of
interest in both the Emacs and Scheme communities for an Emacs clone
written in Scheme. Our job is to coordinate everyone's efforts, and to
make it as easy as possible for anyone to contribute. Please feel free
to get in touch with us, we want to help you contribute code.

If you would like some ideas on how to contribute, a good place to
start is to read our [**feature wish list**](../../../wiki/WishList.md) document.

Also check out issue [#30 "*Get Emacs Regression Tests (ERT) to evaluate on the `cl-lib.el`*"](https://codeberg.org/ramin_hal9001/schemacs/issues/30) if you would
like to try contributing to the Emacs Lisp interpreter part of this project.
