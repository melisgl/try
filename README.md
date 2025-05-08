<a id="x-28TRY-3A-40TRY-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# Try Manual

## Table of Contents

- [1 Links and Systems][628a]
- [2 Tutorial][b949]
- [3 Emacs Integration][4c86]
    - [3.1 Emacs Setup][4fc4]
- [4 Events][aaf2]
    - [4.1 Middle Layer of Events][3e0c]
    - [4.2 Concrete Events][279a]
    - [4.3 Event Glue][5237]
    - [4.4 Printing Events][afb9]
    - [4.5 Event Restarts][d4ce]
    - [4.6 Outcomes][e514]
        - [4.6.1 Outcome Restarts][7ef5]
        - [4.6.2 Checks][bb56]
            - [4.6.2.1 Check Restarts][2364]
        - [4.6.3 Trials][e6be]
            - [4.6.3.1 Trial Events][514a]
            - [4.6.3.2 Trial Verdicts][5e1a]
            - [4.6.3.3 Trial Restarts][5355]
    - [4.7 Errors][7f8e]
    - [4.8 Categories][03ec]
- [5 The `IS` Macro][e2e0]
    - [5.1 Format Specifier Forms][3233]
    - [5.2 Captures][3d27]
        - [5.2.1 Automatic Captures][56ae]
            - [5.2.1.1 Writing Automatic Capture Rules][0743]
        - [5.2.2 Explicit Captures][20d8]
- [6 Check Library][f7f7]
    - [6.1 Checking Conditions][ff2c]
    - [6.2 Miscellaneous Checks][37c1]
    - [6.3 Check Utilities][906a]
        - [6.3.1 Comparing Floats][9fa9]
- [7 Tests][dc28]
    - [7.1 Calling Test Functions][012f]
    - [7.2 Explicit `TRY`][1720]
        - [7.2.1 Testables][cdc3]
        - [7.2.2 Implementation of Implicit `TRY`][8b9c]
    - [7.3 Printing Events][b3f9]
    - [7.4 Counting Events][886e]
    - [7.5 Collecting Events][52e5]
    - [7.6 Rerunning Trials][e4ac]
    - [7.7 Reprocessing Trials][2337]
- [8 Implementation Notes][3eef]
- [9 Glossary][c759]

###### \[in package TRY\]
<a id="x-28TRY-3A-40LINKS-20MGL-PAX-3ASECTION-29"></a>

## 1 Links and Systems

Here is the [official repository](https://github.com/melisgl/try)
and the [HTML
documentation](http://melisgl.github.io/mgl-pax-world/try-manual.html)
for the latest version.

<a id="x-28-22try-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

- [system] **"try"**
    - _Version:_ 0.0.1
    - _Description:_ Try is an extensible test framework with equal support
        for interactive and non-interactive workflows.
    - _Long Description:_ Try stays as close to normal Lisp evaluation
        rules as possible. Tests are functions that record the checks they
        perform as events. These events provide the means of customization
        of what to debug, print, rerun. There is a single fundamental check,
        the extensible [`IS`][80d6] macro. Everything else is built on top.
    - _Licence:_ MIT, see COPYING.
    - _Author:_ Gábor Melis
    - _Mailto:_ [mega@retes.hu](mailto:mega@retes.hu)
    - _Homepage:_ [http://github.com/melisgl/try](http://github.com/melisgl/try)
    - _Bug tracker:_ [https://github.com/melisgl/try/issues](https://github.com/melisgl/try/issues)
    - _Source control:_ [GIT](https://github.com/melisgl/try.git)
    - *Depends on:* alexandria, cl-ppcre, closer-mop, ieee-floats, mgl-pax, trivial-gray-streams, uiop
    - *Defsystem depends on:* try.asdf

<a id="x-28TRY-3A-40TUTORIAL-20MGL-PAX-3ASECTION-29"></a>

## 2 Tutorial

Try is a library for unit testing with equal support for
interactive and non-interactive workflows. Tests are functions, and
almost everything else is a condition, whose types feature
prominently in parameterization.

Try is is what we get if we make tests functions and build a test
framework on top of the condition system as
[Stefil](https://common-lisp.net/project/stefil/index-old.shtml) did
but also address the issue of rerunning and replaying, make the [`IS`][80d6]
check more capable, use the types of the condition hierarchy to
parametrize what to debug, print, rerun, and finally document the
whole thing.

##### Looking for Truth

[The `IS` Macro][e2e0] is a replacement for [`CL:ASSERT`][97ee], that can capture values of
subforms to provide context to failures:

```common-lisp
(is (= (1+ 5) 0))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (= #1=(1+ 5) 0))
..   where
..     #1# = 6
```

This is a PAX transcript,
output is prefixed with `..`. Readable and unreadable return values
are prefixed with `=>` and `==>`, respectively.

Note the `#N#` syntax due to [`*PRINT-CIRCLE*`][c8cb].

##### Checking Multiple Values

`IS` [automatically captures][56ae] values of arguments
to functions like [`1+`][1eb3] in the above example. Values of other
interesting subforms can be [explicitly captured][20d8]. `IS` supports capturing multiple values and can
be taught [how to deal with macros][0743]. The combination of these
features allows [`MATCH-VALUES`][162a] to be implementable as tiny extension:

```common-lisp
(is (match-values (values (1+ 5) "sdf")
      (= * 0)
      (string= * "sdf")))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS
..      (MATCH-VALUES #1=(VALUES (1+ 5) #2="sdf")
..        (= * 0)
..        (STRING= * "sdf")))
..   where
..     #1# == 6
..            #2#
```

In the body of `MATCH-VALUES`, `*` is bound to
successive return values of some form, here `(VALUES (1+ 5) "sdf")`.
`MATCH-VALUES` comes with an automatic rewrite rule that captures the
values of this form, which are printed above as `#1# == 6 #2#`. `IS`
is flexible enough that all other checks ([`SIGNALS`][6d4e], [`SIGNALS-NOT`][7af9],
[`INVOKES-DEBUGGER`][12ce], [`INVOKES-DEBUGGER-NOT`][aaaa], [`FAILS`][e80e], and [`IN-TIME`][f3af] are built
on top of it.

##### Writing Tests

Beyond `IS`, a fancy `ASSERT`, Try provides tests, which are Lisp
functions that record their execution in [`TRIAL`][99d0] objects. Let's define
a test and run it:

```common-lisp
(deftest should-work ()
  (is t))

(should-work)
.. SHOULD-WORK            ; TRIAL-START
..   ⋅ (IS T)             ; EXPECTED-RESULT-SUCCESS
.. ⋅ SHOULD-WORK ⋅1       ; EXPECTED-VERDICT-SUCCESS
..
==> #<TRIAL (SHOULD-WORK) EXPECTED-SUCCESS 0.000s ⋅1>
```

Try is driven by conditions, and the comments to the right give the
type of the condition that is printed on that line. The `⋅`
character marks successes.

We could have run our test with `(TRY 'SHOULD-WORK)` as well, which
does pretty much the same thing except it defaults to never entering
the debugger, whereas calling a test function directly enters the
debugger on events whose type matches the type in the variable
[`*DEBUG*`][856d].

```common-lisp
(try 'should-work)
.. SHOULD-WORK
..   ⋅ (IS T)
.. ⋅ SHOULD-WORK ⋅1
..
==> #<TRIAL (SHOULD-WORK) EXPECTED-SUCCESS 0.000s ⋅1>
```

##### Test Suites

Test suites are just tests that call other tests.

```common-lisp
(deftest my-suite ()
  (should-work)
  (is (= (foo) 5)))

(defun foo ()
  4)

(try 'my-suite)
.. MY-SUITE                 ; TRIAL-START
..   SHOULD-WORK            ; TRIAL-START
..     ⋅ (IS T)             ; EXPECTED-RESULT-SUCCESS
..   ⋅ SHOULD-WORK ⋅1       ; EXPECTED-VERDICT-SUCCESS
..   ⊠ (IS (= #1=(FOO) 5))  ; UNEXPECTED-RESULT-FAILURE
..     where
..       #1# = 4
.. ⊠ MY-SUITE ⊠1 ⋅1         ; UNEXPECTED-VERDICT-FAILURE
..
==> #<TRIAL (MY-SUITE) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅1>
```

`⊠` marks [`UNEXPECTED-FAILURE`][b5cb]s. Note how the failure of `(IS (= (FOO)
5))` caused `MY-SUITE` to fail as well. Finally, the `⊠1` and the
`⋅1` in the `TRIAL`'s printed representation are the [event
counts][886e].

##### Filtering Output

To focus on the important bits, we can print only the [`UNEXPECTED`][d6ad]
events:

```common-lisp
(try 'my-suite :print 'unexpected)
.. MY-SUITE
..   ⊠ (IS (= #1=(FOO) 5))
..     where
..       #1# = 4
.. ⊠ MY-SUITE ⊠1 ⋅1
..
==> #<TRIAL (MY-SUITE) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅1>
```

Note that `SHOULD-WORK` is still run, and its check's success is
counted as evidenced by`⋅1`. The above effect can also be achieved
without running the tests again with [`REPLAY-EVENTS`][8b69].

##### Debugging

Let's figure out what went wrong:

```
(my-suite)

;;; Here the debugger is invoked:
UNEXPECTED-FAILURE in check:
  (IS (= #1=(FOO) 5))
where
  #1# = 4
Restarts:
 0: [RECORD-EVENT] Record the event and continue.
 1: [FORCE-EXPECTED-SUCCESS] Change outcome to TRY:EXPECTED-RESULT-SUCCESS.
 2: [FORCE-UNEXPECTED-SUCCESS] Change outcome to TRY:UNEXPECTED-RESULT-SUCCESS.
 3: [FORCE-EXPECTED-FAILURE] Change outcome to TRY:EXPECTED-RESULT-FAILURE.
 4: [ABORT-CHECK] Change outcome to TRY:RESULT-ABORT*.
 5: [SKIP-CHECK] Change outcome to TRY:RESULT-SKIP.
 6: [RETRY-CHECK] Retry check.
 7: [ABORT-TRIAL] Record the event and abort trial TRY::MY-SUITE.
 8: [SKIP-TRIAL] Record the event and skip trial TRY::MY-SUITE.
 9: [RETRY-TRIAL] Record the event and retry trial TRY::MY-SUITE.
 10: [SET-TRY-DEBUG] Supply a new value for :DEBUG of TRY:TRY.
 11: [RETRY] Retry SLIME interactive evaluation request.
```

In the [SLIME](https://common-lisp.net/project/slime/doc/html/)
debugger, we press `v` on the frame of the call to `MY-SUITE` to
navigate to its definition, realize what the problem is and fix
`FOO`:

```common-lisp
(defun foo ()
  5)
```

Now, we select the [`RETRY-TRIAL`][fae3] restart, and on the retry
`MY-SUITE` passes. The full output is:

```
MY-SUITE
  SHOULD-WORK
    ⋅ (IS T)
  ⋅ SHOULD-WORK ⋅1
WARNING: redefining TRY::FOO in DEFUN
  ⊠ (IS (= #1=(FOO) 5))
    where
      #1# = 4
MY-SUITE retry #1
  SHOULD-WORK
    ⋅ (IS T)
  ⋅ SHOULD-WORK ⋅1
  ⋅ (IS (= (FOO) 5))
⋅ MY-SUITE ⋅2
```

##### Rerunning Stuff

Instead of working interactively, one can fix the failing test and
rerun it. Now, let's fix `MY-SUITE` and rerun it:

```common-lisp
(deftest my-suite ()
  (should-work)
  (is nil))

(try 'my-suite)
.. MY-SUITE
..   SHOULD-WORK
..     ⋅ (IS T)
..   ⋅ SHOULD-WORK ⋅1
..   ⊠ (IS NIL)
.. ⊠ MY-SUITE ⊠1 ⋅1
..
==> #<TRIAL (MY-SUITE) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅1>

(deftest my-suite ()
  (should-work)
  (is t))

(try !)
.. MY-SUITE
..   ⋅ (IS T)
.. ⋅ MY-SUITE ⋅1
..
==> #<TRIAL (MY-SUITE) EXPECTED-SUCCESS 0.004s ⋅1>
```

Here, [`!`][92af] refers to the most recent `TRIAL` returned by [`TRY`][b602]. When a
trial is passed to `TRY` or is [`FUNCALL`][03c7]ed, trials in it that match
the type in `TRY`'s `RERUN` argument are rerun (here, `UNEXPECTED` by
default). `SHOULD-WORK` and its check are [`EXPECTED-SUCCESS`][c96a]es,
hence they don't match `UNEXPECTED` and are not [rerun][e4ac].

##### Conditional Execution

Conditional execution can be achieved simply testing the `TRIAL`
object returned by [Tests][dc28].

```
(deftest my-suite ()
  (when (passedp (should-work))
    (is t :msg "a test that depends on SHOULD-WORK")
    (when (is nil)
      (is nil :msg "never run"))))
```

##### Skipping

Sometimes, we do not know up front that a test should not be
executed. Calling [`SKIP-TRIAL`][f45a] unwinds from the [`CURRENT-TRIAL`][e186] and sets
it skipped.

```common-lisp
(deftest my-suite ()
  (is t)
  (skip-trial)
  (is nil))

(my-suite)
==> #<TRIAL (MY-SUITE) SKIP 0.000s ⋅1>
```

In the above, `(IS T)` was executed, but `(IS NIL)` was not.

##### Expecting Outcomes

```common-lisp
(deftest known-broken ()
  (with-failure-expected (t)
    (is nil)))

(known-broken)
.. KNOWN-BROKEN
..   × (IS NIL)
.. ⋅ KNOWN-BROKEN ×1
..
==> #<TRIAL (KNOWN-BROKEN) EXPECTED-SUCCESS 0.000s ×1>
```

`×` marks [`EXPECTED-FAILURE`][8620]s. `(WITH-SKIP (T) ...)` makes all checks
successes and failures [`EXPECTED`][b194], which are counted in their own
[`*CATEGORIES*`][e949] by default but don't make the enclosing tests to fail.
Also see [`WITH-EXPECTED-OUTCOME`][1d97].

##### Running Tests on Definition

With [`*RUN-DEFTEST-WHEN*`][cfd3], tests on in various [`EVAL-WHEN`][9c9c] situations.
To run tests on evaluation, as in SLIME `C-M-x`, `slime-eval-defun`:

```common-lisp
(setq *run-deftest-when* :execute)

(deftest some-test ()
  (is t))
.. SOME-TEST
..   ⋅ (IS T)
.. ⋅ SOME-TEST ⋅1
..
=> SOME-TEST

(setq *run-deftest-when* nil)
```

##### Fixtures

There is no direct support for fixtures in Try. One can easily write
macros like the following.

```
(defvar *server* nil)

(defmacro with-xxx (&body body)
  `(flet ((,with-xxx-body ()
            ,@body))
     (if *server*
         (with-xxx-body)
         (with-server (make-expensive-server)
           (with-xxx-body)))))
```

Plus, with support for selectively [Rerunning Trials][e4ac], the need for fixtures is
lessened.

##### Packages

The suggested way of writing tests is to call test functions
explicitly:

```
(defpackage :some-test-package
  (:use #:common-lisp #:try))
(in-package :some-test-package)

(deftest test-all ()
  (test-this)
  (test-that))

(deftest test-this ()
  (test-this/more))

(deftest test-this/more ()
  (is t))

(deftest test-that ()
  (is t))

(deftest not-called ()
  (is t))

(defun test ()
  (warn-on-tests-not-run ((find-package :some-test-package))
    (try 'test-all)))

(test)
.. TEST-ALL
..   TEST-THIS
..     TEST-THIS/MORE
..       ⋅ (IS T)
..     ⋅ TEST-THIS/MORE ⋅1
..   ⋅ TEST-THIS ⋅1
..   TEST-THAT
..     ⋅ (IS T)
..   ⋅ TEST-THAT ⋅1
.. ⋅ TEST-ALL ⋅2
.. WARNING: Test NOT-CALLED not run.
==> #<TRIAL (TEST-ALL) EXPECTED-SUCCESS 0.012s ⋅2>
```

Note how the `TEST` function uses [`WARN-ON-TESTS-NOT-RUN`][5289] to catch any
tests defined in `SOME-TEST-PACKAGE` that were not run. Tests can be
deleted by [`FMAKUNBOUND`][609c], [`UNINTERN`][cdba], or by redefining the function with
[`DEFUN`][f472]. Tests defined in a given package can be listed with
[`LIST-PACKAGE-TESTS`][b426].

This style allows higher level tests to establish the dynamic
environment necessary for lower level tests.

<a id="x-28TRY-3A-40EMACS-20MGL-PAX-3ASECTION-29"></a>

## 3 Emacs Integration

The Elisp `mgl-try` interactive command runs a Try test and
displays its output in a `lisp-mode` buffer with minor modes
`outline-mode` and `mgl-try-mode`. It is assumed that the Lisp is
running under [Slime](https://slime.common-lisp.dev/). In the
buffer, the following key bindings are available.

- `M-.` to visit a test function.

- `p` and `n` to move between [`UNEXPECTED`][d6ad] events.

- `P` and `N` to move between events which are not
  [`EXPECTED-SUCCESS`][c96a]es.

- `t` to run an arbitrary test (defaults to symbol under point).
  With a prefix arg, the test is called directly (see
  [Calling Test Functions][012f]) with no arguments.

- `r` to rerun the most recent trial ([`TRY:!`][92af]), subject to the
  filtering described [Rerunning Trials][e4ac]. With a prefix arg, the test is called
  directly.

- `R` to rerun the most recently finished test (and all tests it
  calls). With a prefix arg, the test is called directly.

- some low-level outline mode commands are also given convenient
  bindings:

        <tab>           outline-cycle
        C-p             outline-previous-visible-heading
        C-n             outline-next-visible-heading
        U               outline-up-heading


<a id="x-28TRY-3A-40EMACS-SETUP-20MGL-PAX-3ASECTION-29"></a>

### 3.1 Emacs Setup

Load `src/mgl-try.el` in Emacs.

If you installed Try with Quicklisp, the location of `mgl-try.el`
may change with updates, and you may want to copy the current
version of `mgl-try.el` to a stable location:

    (try:install-try-elisp "~/quicklisp/")

Then, assuming the Elisp file is in the quicklisp directory, add
something like this to your `.emacs`:

```elisp
(load "~/quicklisp/mgl-try.el")
```

For easy access to the functionality of the keys `t`, `r` and `R`
described in [Emacs Integration][4c86], you may want give them a global binding:

```elisp
(global-set-key (kbd "s-t t") 'mgl-try)
(global-set-key (kbd "s-t r") 'mgl-try-rerun-!)
(global-set-key (kbd "s-t R") 'mgl-try-rerun-!-all)
```


<a id="x-28TRY-3AINSTALL-TRY-ELISP-20FUNCTION-29"></a>

- [function] **INSTALL-TRY-ELISP** *TARGET-DIR*

    Copy `mgl-try.el` distributed with this package to `TARGET-DIR`.

<a id="x-28TRY-3A-40EVENTS-20MGL-PAX-3ASECTION-29"></a>

## 4 Events

Try is built around events implemented as [`CONDITION`][83e1]s.
Matching the types of events to [`*DEBUG*`][856d], [`*COUNT*`][3bb4], [`*COLLECT*`][307c], [`*RERUN*`][63db],
[`*PRINT*`][7ee9], and [`*DESCRIBE*`][aa6d] is what gives Try its flexibility.

<a id="x-28TRY-3A-40MIDDLE-LAYER-OF-EVENTS-20MGL-PAX-3ASECTION-29"></a>

### 4.1 Middle Layer of Events

The event hierarchy is fairly involved, so let's start with the middle
layer because it is smallest. The condition [`EVENT`][955d] has 4 disjoint
subclasses:

- [`TRIAL-START`][b664], which corresponds to the entry to a test (see
  [Tests][dc28]),

- [`VERDICT`][52e1], the [`OUTCOME`][2656] of a [`TRIAL`][99d0],

- [`RESULT`][231f], the `OUTCOME` of a check (see [Checks][bb56]), and

- [`ERROR*`][0321], an unexpected `CL:ERROR`([`0`][d162] [`1`][35ba]) or unadorned [non-local exit][b815].

```common-lisp
(let (;; We don't want to debug nor print a backtrace for the error below.
      (*debug* nil)
      (*describe* nil))
  ;; signals TRIAL-START / VERDICT-ABORT* on entry / exit
  (with-test (demo)
    ;; signals EXPECTED-RESULT-SUCCESS
    (is t)
    ;; signals UNHANDLED-ERROR with a nested CL:ERROR
    (error "xxx")))
.. DEMO                       ; TRIAL-START
..   ⋅ (IS T)                 ; EXPECTED-RESULT-SUCCESS (⋅)
..   ⊟ "xxx" (SIMPLE-ERROR)   ; UNHANDLED-ERROR (⊟)
.. ⊟ DEMO ⊟1 ⋅1               ; VERDICT-ABORT* (⊟)
..
==> #<TRIAL (WITH-TEST (DEMO)) ABORT* 0.004s ⊟1 ⋅1>
```


<a id="x-28TRY-3A-40CONCRETE-EVENTS-20MGL-PAX-3ASECTION-29"></a>

### 4.2 Concrete Events

The non-abstract condition classes of events that are actually
signalled are called concrete.

[`TRIAL-START`][b664] is a concrete event class. [`RESULT`][231f]s and [`VERDICT`][52e1]s have six
 concrete subclasses:

- [`EXPECTED-RESULT-SUCCESS`][609c7], [`UNEXPECTED-RESULT-SUCCESS`][b72c],
   [`EXPECTED-RESULT-FAILURE`][d619], [`UNEXPECTED-RESULT-FAILURE`][daeb],
   [`RESULT-SKIP`][7c3f], [`RESULT-ABORT*`][ffab]

- [`EXPECTED-VERDICT-SUCCESS`][06c2], [`UNEXPECTED-VERDICT-SUCCESS`][062e],
   [`EXPECTED-VERDICT-FAILURE`][30c9], [`UNEXPECTED-VERDICT-FAILURE`][fdf4],
   [`VERDICT-SKIP`][5786], [`VERDICT-ABORT*`][4805]

[`ERROR*`][0321] is an abstract class with two concrete subclasses:

- [`UNHANDLED-ERROR`][8f78], signalled when a `CL:ERROR`([`0`][d162] [`1`][35ba]) reaches the handler set
   up by [`DEFTEST`][e7ca] or [`WITH-TEST`][8f5d], or when the debugger is invoked.

- [`NLX`][b115], signalled when no error was detected by the handler, but the
   trial finishes with a [non-local exit][b815].

These are the 15 concrete event classes.

<a id="x-28TRY-3A-40EVENT-GLUE-20MGL-PAX-3ASECTION-29"></a>

### 4.3 Event Glue

These condition classes group various bits of the [Concrete Events][279a]
 and the [Middle Layer of Events][3e0c] for ease of reference.

Concrete event classes except [`TRIAL-START`][b664] are subclasses of the
hyphen-separated words constituting their name. For example,
[`UNEXPECTED-RESULT-FAILURE`][daeb] inherits from [`UNEXPECTED`][d6ad], [`RESULT`][231f], and
[`FAILURE`][f92d], so it matches types such as `UNEXPECTED` or `(AND UNEXPECTED
RESULT)`.

<a id="x-28TRY-3AEVENT-20CONDITION-29"></a>

- [condition] **EVENT**

    Common abstract superclass of all events in Try.

<a id="x-28TRY-3AEXPECTED-20CONDITION-29"></a>

- [condition] **EXPECTED** *[EVENT][955d]*

    Concrete condition classes with `EXPECTED` in their
    name are subclasses of `EXPECTED`. [`SKIP`][69a2] is also a subclass of
    `EXPECTED`.

<a id="x-28TRY-3AUNEXPECTED-20CONDITION-29"></a>

- [condition] **UNEXPECTED** *[EVENT][955d]*

    Concrete condition classes with `UNEXPECTED` in their
    name are subclasses of `UNEXPECTED`. [`ABORT*`][8ec3] is also a subclass of
    `UNEXPECTED`.

<a id="x-28TRY-3ASUCCESS-20CONDITION-29"></a>

- [condition] **SUCCESS** *[EVENT][955d]*

    See [Checks][bb56] and [Trial Verdicts][5e1a] for how
    `SUCCESS` or [`FAILURE`][f92d] is decided.

<a id="x-28TRY-3AFAILURE-20CONDITION-29"></a>

- [condition] **FAILURE** *[EVENT][955d]*

    See [`SUCCESS`][269a].

<a id="x-28TRY-3ADISMISSAL-20CONDITION-29"></a>

- [condition] **DISMISSAL** *[EVENT][955d]*

    The third possibility after [`SUCCESS`][269a] and [`FAILURE`][f92d].
    Either [`SKIP`][69a2] or [`ABORT*`][8ec3].

<a id="x-28TRY-3AABORT-2A-20CONDITION-29"></a>

- [condition] **ABORT\*** *[UNEXPECTED][d6ad]*

    [`RESULT-ABORT*`][ffab], [`VERDICT-ABORT*`][4805] or [`ERROR*`][0321].

<a id="x-28TRY-3ASKIP-20CONDITION-29"></a>

- [condition] **SKIP** *[EXPECTED][b194] [DISMISSAL][0992]*

    [`RESULT-SKIP`][7c3f] or [`VERDICT-SKIP`][5786].

<a id="x-28TRY-3ALEAF-20CONDITION-29"></a>

- [condition] **LEAF** *[EVENT][955d]*

    [`RESULT`][231f] or [`ERROR*`][0321].

<a id="x-28TRY-3AEXPECTED-SUCCESS-20TYPE-29"></a>

- [type] **EXPECTED-SUCCESS**

    A shorthand for `(AND EXPECTED SUCCESS)`.

<a id="x-28TRY-3AUNEXPECTED-SUCCESS-20TYPE-29"></a>

- [type] **UNEXPECTED-SUCCESS**

    A shorthand for `(AND UNEXPECTED SUCCESS)`.

<a id="x-28TRY-3AEXPECTED-FAILURE-20TYPE-29"></a>

- [type] **EXPECTED-FAILURE**

    A shorthand for `(AND EXPECTED FAILURE)`.

<a id="x-28TRY-3AUNEXPECTED-FAILURE-20TYPE-29"></a>

- [type] **UNEXPECTED-FAILURE**

    A shorthand for `(AND UNEXPECTED FAILURE)`.

<a id="x-28TRY-3APASS-20TYPE-29"></a>

- [type] **PASS**

    An [`OUTCOME`][2656] that's not an [`ABORT*`][8ec3] or an [`UNEXPECTED`][d6ad] [`FAILURE`][f92d].
    `PASS` is equivalent to `(NOT FAIL)`.

<a id="x-28TRY-3AFAIL-20TYPE-29"></a>

- [type] **FAIL**

    An [`ABORT*`][8ec3] or an [`UNEXPECTED`][d6ad] [`FAILURE`][f92d]. See [`PASS`][21d9].

<a id="x-28TRY-3A-40PRINTING-EVENTS-20MGL-PAX-3ASECTION-29"></a>

### 4.4 Printing Events

<a id="x-28TRY-3A-2AEVENT-PRINT-BINDINGS-2A-20VARIABLE-29"></a>

- [variable] **\*EVENT-PRINT-BINDINGS\*** *((\*PRINT-CIRCLE\* T))*

    [`EVENT`][955d]s are conditions signalled in code that may change printer
    variables such as [`*PRINT-CIRCLE*`][c8cb], [`*PRINT-LENGTH*`][8f7a], etc. To control
    how events are printed, the list of variable bindings in
    `*EVENT-PRINT-BINDINGS*` is established whenever an `EVENT` is printed
    as if with:
    
    ```
    (progv (mapcar #'first *event-print-bindings*)
           (mapcar #'second *event-print-bindings*)
      ...)
    ```
    
    The default value ensures that shared structure is recognized (see
    [Captures][3d27]). If the `#N#` syntax feels cumbersome, then change this
    variable.

<a id="x-28TRY-3A-40EVENT-RESTARTS-20MGL-PAX-3ASECTION-29"></a>

### 4.5 Event Restarts

Only [`RECORD-EVENT`][ce49] is applicable to all [`EVENT`][955d]s. See
[Check Restarts][2364], [Trial Restarts][5355] for more.

<a id="x-28TRY-3ARECORD-EVENT-20FUNCTION-29"></a>

- [function] **RECORD-EVENT** *&OPTIONAL CONDITION*

    This restart is always the first restart available when an [`EVENT`][955d] is
    signalled running under [`TRY`][b602] (i.e. there is a [`CURRENT-TRIAL`][e186]). `TRY`
    always invokes `RECORD-EVENT` when handling events.

<a id="x-28TRY-3A-40OUTCOMES-20MGL-PAX-3ASECTION-29"></a>

### 4.6 Outcomes

<a id="x-28TRY-3AOUTCOME-20CONDITION-29"></a>

- [condition] **OUTCOME** *[EVENT][955d]*

    An `OUTCOME` is the resolution of either a [`TRIAL`][99d0] or a
    check (see [Checks][bb56]), corresponding to subclasses [`VERDICT`][52e1] and [`RESULT`][231f].

<a id="x-28TRY-3AWITH-EXPECTED-OUTCOME-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-EXPECTED-OUTCOME** *(EXPECTED-TYPE) &BODY BODY*

    When an [`OUTCOME`][2656] is to be signalled, `EXPECTED-TYPE` determines
    whether it's going to be [`EXPECTED`][b194]. The concrete `OUTCOME` classes are
    `{EXPECTED,UNEXPECTED}-{RESULT,VERDICT}-{SUCCESS,FAILURE}` (see
    [Events][aaf2]), of which [`RESULT`][231f] or [`VERDICT`][52e1] and [`SUCCESS`][269a] or [`FAILURE`][f92d] are
    already known. If a `RESULT` `FAILURE` is to be signalled, then the
    moral equivalent of `(SUBTYPEP '(AND RESULT FAILURE) EXPECTED-TYPE)`
    is evaluated and depending on whether it's true,
    [`EXPECTED-RESULT-FAILURE`][d619] or [`UNEXPECTED-RESULT-FAILURE`][daeb] is signalled.
    
    By default, `SUCCESS` is expected. The following example shows how to
    expect both `SUCCESS` and `FAILURE` for `RESULT`s, while requiring
    `VERDICT`s to succeed:
    
    ```common-lisp
    (let ((*debug* nil))
      (with-expected-outcome ('(or result (and verdict success)))
        (with-test (t1)
          (is nil))))
    .. T1
    ..   × (IS NIL)
    .. ⋅ T1 ×1
    ..
    ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-SUCCESS 0.000s ×1>
    ```
    
    This is equivalent to `(WITH-FAILURE-EXPECTED () ...)`. To make
    result failures expected but result successes unexpected:
    
    ```common-lisp
    (let ((*debug* nil))
      (with-expected-outcome ('(or (and result failure) (and verdict success)))
        (with-test (t1)
          (is t)
          (is nil))))
    .. T1
    ..   ⊡ (IS T)
    ..   × (IS NIL)
    .. ⋅ T1 ⊡1 ×1
    ..
    ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-SUCCESS 0.000s ⊡1 ×1>
    ```
    
    This is equivalent to `(WITH-FAILURE-EXPECTED ('FAILURE) ...)`. The
    final example leaves result failures unexpected but makes both
    verdict successes and failures expected:
    
    ```common-lisp
    (let ((*debug* nil))
      (with-expected-outcome ('(or (and result success) verdict))
        (with-test (t1)
          (is nil))))
    .. T1
    ..   ⊠ (IS NIL)
    .. × T1 ⊠1
    ..
    ==> #<TRIAL (WITH-TEST (T1)) EXPECTED-FAILURE 0.004s ⊠1>
    ```


<a id="x-28TRY-3AWITH-FAILURE-EXPECTED-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-FAILURE-EXPECTED** *(&OPTIONAL (RESULT-EXPECTED-TYPE T) (VERDICT-EXPECTED-TYPE ''SUCCESS)) &BODY BODY*

    A convenience macro on top of [`WITH-EXPECTED-OUTCOME`][1d97],
    `WITH-FAILURE-EXPECTED` expects [`VERDICT`][52e1]s to have `VERDICT-EXPECTED-TYPE`
    and [`RESULT`][231f]s to have `RESULT-EXPECTED-TYPE`. A simple
    `(WITH-FAILURE-EXPECTED () ...)` makes all `RESULT` [`SUCCESS`][269a]es and
    [`FAILURE`][f92d]s [`EXPECTED`][b194]. `(WITH-FAILURE-EXPECTED ('FAILURE) ..)` expects
    `FAILURE`s only, and any `SUCCESS`es will be [`UNEXPECTED`][d6ad].

<a id="x-28TRY-3AWITH-SKIP-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-SKIP** *(&OPTIONAL (SKIP T)) &BODY BODY*

    `WITH-SKIP` skips checks and trials. It forces an immediate
    [`SKIP-TRIAL`][f45a] whenever a trial is started (which turns into a
    [`VERDICT-SKIP`][5786]) and makes checks (without intervening trials, of
    course) evaluate normally but signal [`RESULT-SKIP`][7c3f]. `SKIP` is `NIL`
    cancels the effect of any enclosing `WITH-SKIP` with `SKIP` true.

<a id="x-28TRY-3A-40OUTCOME-RESTARTS-20MGL-PAX-3ASECTION-29"></a>

#### 4.6.1 Outcome Restarts

<a id="x-28TRY-3AFORCE-EXPECTED-SUCCESS-20FUNCTION-29"></a>

- [function] **FORCE-EXPECTED-SUCCESS** *&OPTIONAL CONDITION*

    Change the type of the [`OUTCOME`][2656] being signalled to [`EXPECTED`][b194] and
    [`SUCCESS`][269a]. If the original condition is a [`RESULT`][231f], then this will be
    [`EXPECTED-RESULT-SUCCESS`][609c7], if it is a [`VERDICT`][52e1], then
    [`EXPECTED-VERDICT-SUCCESS`][06c2].

<a id="x-28TRY-3AFORCE-UNEXPECTED-SUCCESS-20FUNCTION-29"></a>

- [function] **FORCE-UNEXPECTED-SUCCESS** *&OPTIONAL CONDITION*

    Change the type of [`OUTCOME`][2656] being signalled to [`UNEXPECTED`][d6ad] and
    [`SUCCESS`][269a].

<a id="x-28TRY-3AFORCE-EXPECTED-FAILURE-20FUNCTION-29"></a>

- [function] **FORCE-EXPECTED-FAILURE** *&OPTIONAL CONDITION*

    Change the type of [`OUTCOME`][2656] being signalled to [`EXPECTED`][b194] and
    [`FAILURE`][f92d].

<a id="x-28TRY-3AFORCE-UNEXPECTED-FAILURE-20FUNCTION-29"></a>

- [function] **FORCE-UNEXPECTED-FAILURE** *&OPTIONAL CONDITION*

    Change the type of [`OUTCOME`][2656] being signalled to [`UNEXPECTED`][d6ad] and
    [`FAILURE`][f92d].

<a id="x-28TRY-3A-40CHECKS-20MGL-PAX-3ASECTION-29"></a>

#### 4.6.2 Checks

Checks are like [`CL:ASSERT`][97ee]s, they check whether some condition holds
and signal an [`OUTCOME`][2656]. The outcome signalled for checks is a
subclass of [`RESULT`][231f].

Take, for example, `(IS (= X 5))`. Depending on whether `X` is
indeed 5, some kind of `RESULT` [`SUCCESS`][269a] or [`FAILURE`][f92d] will be signalled.
[`WITH-EXPECTED-OUTCOME`][1d97] determines whether it's [`EXPECTED`][b194] or
[`UNEXPECTED`][d6ad], and we have one of [`EXPECTED-RESULT-SUCCESS`][609c7],
[`UNEXPECTED-RESULT-SUCCESS`][b72c], [`EXPECTED-RESULT-FAILURE`][d619],
[`UNEXPECTED-RESULT-FAILURE`][daeb] to signal. Furthermore, if [`WITH-SKIP`][b71e] is in
effect, then [`RESULT-SKIP`][7c3f] is signalled.

The result is signalled with `#'SIGNAL` if it is a [`PASS`][21d9], else it's
signalled with `#'ERROR`. This distinction matters only if the event
is not handled, which is never the case in a [`TRIAL`][99d0]. Standalone
checks though – those not enclosed by a trial – invoke the debugger on
`RESULT`s which are not of type [`PASS`][21d9].

The signalled `RESULT` is not final until [`RECORD-EVENT`][ce49] is invoked on
it, and it can be changed with the [Outcome Restarts][7ef5] and the
[Check Restarts][2364].

<a id="x-28TRY-3ARESULT-20CONDITION-29"></a>

- [condition] **RESULT** *[LEAF][f58d] [OUTCOME][2656]*

<a id="x-28TRY-3AEXPECTED-RESULT-SUCCESS-20CONDITION-29"></a>

- [condition] **EXPECTED-RESULT-SUCCESS** *[EXPECTED][b194] [RESULT][231f] [SUCCESS][269a]*

<a id="x-28TRY-3AUNEXPECTED-RESULT-SUCCESS-20CONDITION-29"></a>

- [condition] **UNEXPECTED-RESULT-SUCCESS** *[UNEXPECTED][d6ad] [RESULT][231f] [SUCCESS][269a]*

<a id="x-28TRY-3AEXPECTED-RESULT-FAILURE-20CONDITION-29"></a>

- [condition] **EXPECTED-RESULT-FAILURE** *[EXPECTED][b194] [RESULT][231f] [FAILURE][f92d]*

<a id="x-28TRY-3AUNEXPECTED-RESULT-FAILURE-20CONDITION-29"></a>

- [condition] **UNEXPECTED-RESULT-FAILURE** *[UNEXPECTED][d6ad] [RESULT][231f] [FAILURE][f92d]*

<a id="x-28TRY-3ARESULT-SKIP-20CONDITION-29"></a>

- [condition] **RESULT-SKIP** *[RESULT][231f] [SKIP][69a2]*

<a id="x-28TRY-3ARESULT-ABORT-2A-20CONDITION-29"></a>

- [condition] **RESULT-ABORT\*** *[RESULT][231f] [ABORT\*][8ec3] [DISMISSAL][0992]*

<a id="x-28TRY-3A-40CHECK-RESTARTS-20MGL-PAX-3ASECTION-29"></a>

##### 4.6.2.1 Check Restarts

<a id="x-28TRY-3AABORT-CHECK-20FUNCTION-29"></a>

- [function] **ABORT-CHECK** *&OPTIONAL CONDITION*

    Change the [`OUTCOME`][2656] of the check being signalled to [`RESULT-ABORT*`][ffab].
    `RESULT-ABORT*`, being a [`FAIL`][d5ea], will cause the check to return `NIL` if
    [`RECORD-EVENT`][ce49] is invoked on it.

<a id="x-28TRY-3ASKIP-CHECK-20FUNCTION-29"></a>

- [function] **SKIP-CHECK** *&OPTIONAL CONDITION*

    Change the [`OUTCOME`][2656] of the check being signalled to [`RESULT-SKIP`][7c3f].
    `RESULT-SKIP`, being a [`PASS`][21d9], will cause the check to return `T` if
    `CONTINUE`([`0`][02a3] [`1`][1867]) or [`RECORD-EVENT`][ce49] is invoked on it.

<a id="x-28TRY-3ARETRY-CHECK-20FUNCTION-29"></a>

- [function] **RETRY-CHECK** *&OPTIONAL CONDITION*

    Initiate a [non-local exit][b815] to go reevaluate the forms
    wrapped by the check without signalling an [`OUTCOME`][2656].

<a id="x-28TRY-3A-40TRIALS-20MGL-PAX-3ASECTION-29"></a>

#### 4.6.3 Trials

<a id="x-28TRY-3ATRIAL-20CLASS-29"></a>

- [class] **TRIAL** *SB-MOP:FUNCALLABLE-STANDARD-OBJECT*

    Trials are records of calls to tests (see
    [Counting Events][886e], [Collecting Events][52e5]). Their behaviour as [funcallable instance][2eef]s
    is explained in [Rerunning Trials][e4ac].
    
    There are three ways to acquire a `TRIAL` object: by calling
    [`CURRENT-TRIAL`][e186], through the lexical binding of the symbol that names
    the test or through the return value of a test:
    
    ```common-lisp
    (deftest xxx ()
      (prin1 xxx))
    
    (xxx)
    .. #<TRIAL (XXX) RUNNING>
    ==> #<TRIAL (XXX) EXPECTED-SUCCESS 0.000s>
    ```
    
    `WITH-TRIAL` can also provide access to its `TRIAL`:
    
    ```common-lisp
    (with-test (t0)
      (prin1 t0))
    .. #<TRIAL (WITH-TEST (T0)) RUNNING>
    ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s>
    ```
    
    `TRIAL`s are not to be instantiated by client code.

<a id="x-28TRY-3ACURRENT-TRIAL-20FUNCTION-29"></a>

- [function] **CURRENT-TRIAL**

    [`TRIAL`][99d0]s, like the calls to tests they stand for, nest. `CURRENT-TRIAL`
    returns the innermost trial. If there is no currently running test,
    then an error is signalled. The returned trial is [`RUNNINGP`][5d4a].

<a id="x-28TRY-3A-40TRIAL-EVENTS-20MGL-PAX-3ASECTION-29"></a>

##### 4.6.3.1 Trial Events

<a id="x-28TRY-3ATRIAL-EVENT-20CONDITION-29"></a>

- [condition] **TRIAL-EVENT** *[EVENT][955d]*

    A `TRIAL-EVENT` is either a [`TRIAL-START`][b664] or a
    [`VERDICT`][52e1].

<a id="x-28TRY-3ATRIAL-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-EVENT-29-29"></a>

- [reader] **TRIAL** *[TRIAL-EVENT][b36a] (:TRIAL)*

<a id="x-28TRY-3ATRIAL-START-20CONDITION-29"></a>

- [condition] **TRIAL-START** *[TRIAL-EVENT][b36a]*

    `TRIAL-START` is signalled when a test function
    (see [Tests][dc28]) is entered and a [`TRIAL`][99d0] is started, it is already the
    [`CURRENT-TRIAL`][e186], and the [Trial Restarts][5355] are available. It is also
    signalled when a trial is retried:
    
    ```common-lisp
    (let ((*print* nil)
          (n 0))
      (with-test ()
        (handler-bind ((trial-start (lambda (c)
                                      (format t "TRIAL-START for ~S retry#~S~%"
                                              (test-name (trial c))
                                              (n-retries (trial c))))))
          (with-test (this)
            (incf n)
            (when (< n 3)
              (retry-trial))))))
    .. TRIAL-START for THIS retry#0
    .. TRIAL-START for THIS retry#1
    .. TRIAL-START for THIS retry#2
    ..
    ```
    
    The matching of `TRIAL-START` events is less straightforward than that
    of other [`EVENT`][955d]s.
    
    - When a `TRIAL-START` event matches the `COLLECT` type (see
      [Collecting Events][52e5]), its [`TRIAL`][0f05] is collected.
    
    - Similarly, when a `TRIAL-START` matches the `PRINT`
      type (see [Printing Events][b3f9]), it is printed immediately, and its trial's
      [`VERDICT`][52e1] will be printed too regardless of whether it matches
      `PRINT`. If `TRIAL-START` does not match
      `PRINT`, it may still be printed if for example
      [`*PRINT-PARENT*`][cc23] requires it.
    
    - When a `TRIAL-START` matches the `RERUN` type (see [Rerunning Trials][e4ac]), its
      [`TRIAL`][0f05] may be rerun.
    
    - Also, see [`WITH-SKIP`][b71e].


<a id="x-28TRY-3AVERDICT-20CONDITION-29"></a>

- [condition] **VERDICT** *[TRIAL-EVENT][b36a] [OUTCOME][2656]*

    A `VERDICT` is the [`OUTCOME`][2656] of a [`TRIAL`][99d0]. It is one of
    `{EXPECTED,UNEXPECTED}-VERDICT-{SUCCESS,FAILURE}`, [`VERDICT-SKIP`][5786] and
    [`VERDICT-ABORT*`][4805]. Regarding how the verdict type is determined, see
    [Trial Verdicts][5e1a].
    
    Verdicts are signalled while their [`TRIAL`][0f05] is
    still the [`CURRENT-TRIAL`][e186], and [Trial Restarts][5355] are still available.
    
    ```common-lisp
    (try (lambda ()
           (handler-bind (((and verdict failure) #'retry-trial))
             (with-test (this)
               (is (zerop (random 2)))))))
    .. (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>)
    ..   THIS
    ..     ⊠ (IS (ZEROP #1=(RANDOM 2)))
    ..       where
    ..         #1# = 1
    ..   THIS retry #1
    ..     ⋅ (IS (ZEROP (RANDOM 2)))
    ..   ⋅ THIS ⋅1
    .. ⋅ (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>) ⋅1
    ..
    ==> #<TRIAL (TRY #<FUNCTION (LAMBDA ()) {53038ADB}>) EXPECTED-SUCCESS 0.000s ⋅1>
    ```


<a id="x-28TRY-3AEXPECTED-VERDICT-SUCCESS-20CONDITION-29"></a>

- [condition] **EXPECTED-VERDICT-SUCCESS** *[EXPECTED][b194] [VERDICT][52e1] [SUCCESS][269a]*

<a id="x-28TRY-3AUNEXPECTED-VERDICT-SUCCESS-20CONDITION-29"></a>

- [condition] **UNEXPECTED-VERDICT-SUCCESS** *[UNEXPECTED][d6ad] [VERDICT][52e1] [SUCCESS][269a]*

<a id="x-28TRY-3AEXPECTED-VERDICT-FAILURE-20CONDITION-29"></a>

- [condition] **EXPECTED-VERDICT-FAILURE** *[EXPECTED][b194] [VERDICT][52e1] [FAILURE][f92d]*

<a id="x-28TRY-3AUNEXPECTED-VERDICT-FAILURE-20CONDITION-29"></a>

- [condition] **UNEXPECTED-VERDICT-FAILURE** *[UNEXPECTED][d6ad] [VERDICT][52e1] [FAILURE][f92d]*

<a id="x-28TRY-3AVERDICT-SKIP-20CONDITION-29"></a>

- [condition] **VERDICT-SKIP** *[VERDICT][52e1] [SKIP][69a2]*

<a id="x-28TRY-3AVERDICT-ABORT-2A-20CONDITION-29"></a>

- [condition] **VERDICT-ABORT\*** *[VERDICT][52e1] [ABORT\*][8ec3] [DISMISSAL][0992]*

<a id="x-28TRY-3A-40TRIAL-VERDICTS-20MGL-PAX-3ASECTION-29"></a>

##### 4.6.3.2 Trial Verdicts

When a trial finished, a [`VERDICT`][52e1] is signalled. The verdict's type
is determined as follows.

- It is a [`VERDICT-SKIP`][5786] if

    - [`SKIP-TRIAL`][f45a] was called on the trial, or

    - [`ABORT-TRIAL`][4f9f], `SKIP-TRIAL`, or [`RETRY-TRIAL`][fae3] was called on an
      enclosing trial, and

    - these were not overruled by a later `ABORT-TRIAL` or `RETRY-TRIAL`
      on the trial.

- It is a [`VERDICT-ABORT*`][4805] if `ABORT-TRIAL` was called on the trial, and
  it wasn't overruled by a later `SKIP-TRIAL` or `RETRY-TRIAL`.

- If all children (including those not collected in [`CHILDREN`][de7d]) of the
  trial [`PASS`][21d9], then the verdict will be a [`SUCCESS`][269a], else it will be a
  [`FAILURE`][f92d].

- Subject to the [`WITH-EXPECTED-OUTCOME`][1d97] in effect,
  `{EXPECTED,UNEXPECTED}-VERDICT-{SUCCESS,FAILURE}` is the type of
  the verdict which will be signalled.

The verdict of this type is signalled, but its type can be changed
by the [Outcome Restarts][7ef5] or the [Trial Restarts][5355] before [`RECORD-EVENT`][ce49]
is invoked on it.

<a id="x-28TRY-3AVERDICT-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29"></a>

- [reader] **VERDICT** *[TRIAL][99d0] (= NIL)*

    The [`VERDICT`][52e1] [`EVENT`][955d] signalled when this
    `TRIAL` finished or `NIL` if it has not finished yet.

<a id="x-28TRY-3ARUNNINGP-20FUNCTION-29"></a>

- [function] **RUNNINGP** *TRIAL*

    See if the function call associated with `TRIAL` has not returned yet.
    Trials that are not running have a [`VERDICT`][52e1] and are said to be
    finished.

<a id="x-28TRY-3APASSEDP-20FUNCTION-29"></a>

- [function] **PASSEDP** *TRIAL*

    See if `TRIAL` has finished and its [`VERDICT`][4bec] is a
    [`PASS`][21d9].

<a id="x-28TRY-3AFAILEDP-20FUNCTION-29"></a>

- [function] **FAILEDP** *TRIAL*

    See if `TRIAL` has finished and its [`VERDICT`][4bec] is a
    [`FAIL`][d5ea].

<a id="x-28TRY-3A-40TRIAL-RESTARTS-20MGL-PAX-3ASECTION-29"></a>

##### 4.6.3.3 Trial Restarts

There are three restarts available for manipulating running
trials: [`ABORT-TRIAL`][4f9f], [`SKIP-TRIAL`][f45a], and [`RETRY-TRIAL`][fae3]. They may be
invoked programmatically or from the debugger. `ABORT-TRIAL` is also
invoked by [`TRY`][b602] when encountering [`UNHANDLED-ERROR`][8f78].

The functions below invoke one of these restarts associated with a
[`TRIAL`][99d0]. It is an error to call them on trials that are not [`RUNNINGP`][5d4a],
but they may be called on trials other than the [`CURRENT-TRIAL`][e186]. In
that case, any intervening trials are skipped.

```common-lisp
;; Skipped trials are marked with '-' in the output.
(with-test (outer)
  (with-test (inner)
    (is t)
    (skip-trial nil outer)))
.. OUTER
..   INNER
..     ⋅ (IS T)
..   - INNER ⋅1
.. - OUTER ⋅1
..
==> #<TRIAL (WITH-TEST (OUTER)) SKIP 0.000s ⋅1>
```

Furthermore, all three restarts initiate a [non-local exit][b815] to
return from the trial. If during the unwinding of the stack, the
non-local-exit is cancelled (see [cancelled non-local exit][7ab6]), the appropriate
restart will be invoked upon returning from the trial. In the
following example, the non-local exit from a skip is cancelled by a
[`THROW`][e760].

```common-lisp
(with-test (some-test)
  (catch 'foo
    (unwind-protect
         (skip-trial)
      (throw 'foo nil)))
  (is t :msg "check after skip"))
.. SOME-TEST
..   ⋅ check after skip
.. - SOME-TEST ⋅1
..
==> #<TRIAL (WITH-TEST (SOME-TEST)) SKIP 0.000s ⋅1>
```

In the next example, the non-local exit from a skip is cancelled by
an `ERROR`([`0`][d162] [`1`][35ba]), which triggers an `ABORT-TRIAL`.

```common-lisp
(let ((*debug* nil)
      (*describe* nil))
  (with-test (foo)
    (unwind-protect
         (skip-trial)
      (error "xxx"))))
.. FOO
..   ⊟ "xxx" (SIMPLE-ERROR)
.. ⊟ FOO ⊟1
..
==> #<TRIAL (WITH-TEST (FOO)) ABORT* 0.000s ⊟1>
```

All three restarts may be invoked on any [`EVENT`][955d], including the
trial's own [`TRIAL-START`][b664] and [`VERDICT`][52e1]. If their `CONDITION`
argument is an `EVENT` (`RETRY-TRIAL` has a special case here), they
also record it (as in [`RECORD-EVENT`][ce49]) to ensure that when they handle
an `EVENT` in the debugger or programmatically that event is not
dropped.

<a id="x-28TRY-3AABORT-TRIAL-20FUNCTION-29"></a>

- [function] **ABORT-TRIAL** *&OPTIONAL CONDITION (TRIAL (CURRENT-TRIAL))*

    Invoke the `ABORT-TRIAL` restart of a [`RUNNINGP`][5d4a] `TRIAL`.
    
    When `CONDITION` is a [`VERDICT`][52e1] for `TRIAL`, `ABORT-TRIAL` signals a new
    verdict of type [`VERDICT-ABORT*`][4805]. This behaviour is similar to that
    of [`ABORT-CHECK`][826a]. Else, the `ABORT-TRIAL` restart may record `CONDITION`,
    then it initiates a [non-local exit][b815] to return from the test
    function with `VERDICT-ABORT*`. If during the unwinding [`SKIP-TRIAL`][f45a]
    or [`RETRY-TRIAL`][fae3] is called, then the abort is cancelled.
    
    Since [`ABORT*`][8ec3] is an [`UNEXPECTED`][d6ad] [`EVENT`][955d], `ABORT-TRIAL` is rarely used
    programmatically. Signalling any error in a trial that's not caught
    before the trial's handler catches it will get turned into an
    [`UNHANDLED-ERROR`][8f78], and [`TRY`][b602] will invoke `ABORT-TRIAL` with it. Thus,
    instead of invoking `ABORT-TRIAL` directly, signalling an error will
    often suffice.

<a id="x-28TRY-3ASKIP-TRIAL-20FUNCTION-29"></a>

- [function] **SKIP-TRIAL** *&OPTIONAL CONDITION (TRIAL (CURRENT-TRIAL))*

    Invoke the `SKIP-TRIAL` restart of a [`RUNNINGP`][5d4a] `TRIAL`.
    
    When `CONDITION` is a [`VERDICT`][52e1] for `TRIAL`, `SKIP-TRIAL` signals a new
    verdict of type [`VERDICT-SKIP`][5786]. This behaviour is similar to that of
    [`SKIP-CHECK`][fb0e]. Else, the `SKIP-TRIAL` restart may record `CONDITION`, then
    it initiates a [non-local exit][b815] to return from the test
    function with `VERDICT-SKIP`. If during the unwinding [`ABORT-TRIAL`][4f9f] or
    [`RETRY-TRIAL`][fae3] is called, then the skip is cancelled.
    
    ```common-lisp
    (with-test (skipped)
      (handler-bind ((unexpected-result-failure #'skip-trial))
        (is nil)))
    .. SKIPPED
    ..   ⊠ (IS NIL)
    .. - SKIPPED ⊠1
    ..
    ==> #<TRIAL (WITH-TEST (SKIPPED)) SKIP 0.000s ⊠1>
    ```
    
    Invoking `SKIP-TRIAL` on the `TRIAL`'s own [`TRIAL-START`][b664] skips the trial
    being started.
    
    ```common-lisp
    (let ((*print* '(or outcome leaf)))
      (with-test (parent)
        (handler-bind ((trial-start #'skip-trial))
          (with-test (child)
            (is nil)))))
    .. PARENT
    ..   - CHILD
    .. ⋅ PARENT
    ..
    ```


<a id="x-28TRY-3ARETRY-TRIAL-20FUNCTION-29"></a>

- [function] **RETRY-TRIAL** *&OPTIONAL CONDITION (TRIAL (CURRENT-TRIAL))*

    Invoke the `RETRY-TRIAL` restart of [`RUNNINGP`][5d4a] `TRIAL`. The `RETRY-TRIAL`
    restart may record `CONDITION`, then it initiates a [non-local
    exit][b815] to go back to the beginning of the test function. If the
    non-local exit completes, then
    
    - ([`N-RETRIES`][b33f] `TRIAL`) is incremented,
    
    - collected results and trials are cleared (see [Collecting Events][52e5]),
    
    - counts are zeroed (see [Counting Events][886e]), and
    
    - [`TRIAL-START`][b664] is signalled again.
    
    If during the unwinding [`ABORT-TRIAL`][4f9f] or [`SKIP-TRIAL`][f45a] is called, then
    the retry is cancelled.
    
    `CONDITION` (which may be `NIL`) is recorded if it is an [`EVENT`][955d] but not
    the [`VERDICT`][52e1] of `TRIAL`, and the [`RECORD-EVENT`][ce49] restart is available.

<a id="x-28TRY-3AN-RETRIES-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29"></a>

- [reader] **N-RETRIES** *[TRIAL][99d0] (:N-RETRIES = 0)*

    The number of times this `TRIAL` has
    been retried. See [`RETRY-TRIAL`][fae3].

<a id="x-28TRY-3A-40ERRORS-20MGL-PAX-3ASECTION-29"></a>

### 4.7 Errors

<a id="x-28TRY-3AERROR-2A-20CONDITION-29"></a>

- [condition] **ERROR\*** *[ABORT\*][8ec3] [LEAF][f58d]*

    Either [`UNHANDLED-ERROR`][8f78] or [`NLX`][b115], `ERROR*` causes or
    represents abnormal termination of a [`TRIAL`][99d0]. [`ABORT-TRIAL`][4f9f] can be
    called with `ERROR*`s, but there is little need for explicitly doing
    so as [`RECORD-EVENT`][ce49], which [`TRY`][b602] invokes, takes care of this.

<a id="x-28TRY-3ATEST-NAME-20-28MGL-PAX-3AREADER-20TRY-3AERROR-2A-29-29"></a>

- [reader] **TEST-NAME** *[ERROR\*][0321] (:TEST-NAME)*

<a id="x-28TRY-3AUNHANDLED-ERROR-20CONDITION-29"></a>

- [condition] **UNHANDLED-ERROR** *[ERROR\*][0321]*

    Signalled when an [`CL:ERROR`][d162] condition reaches the
    handlers set up [`DEFTEST`][e7ca] or [`WITH-TEST`][8f5d], or when their [`*DEBUGGER-HOOK*`][1cdc]
    is invoked with a condition that's not an [`EVENT`][955d].

<a id="x-28TRY-3ANESTED-CONDITION-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29"></a>

- [reader] **NESTED-CONDITION** *[UNHANDLED-ERROR][8f78] (:CONDITION = 'NIL)*

<a id="x-28TRY-3ABACKTRACE-OF-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29"></a>

- [reader] **BACKTRACE-OF** *[UNHANDLED-ERROR][8f78] (:BACKTRACE = 'NIL)*

<a id="x-28TRY-3ADEBUGGER-INVOKED-P-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29"></a>

- [reader] **DEBUGGER-INVOKED-P** *[UNHANDLED-ERROR][8f78] (:DEBUGGER-INVOKED-P = 'NIL)*

<a id="x-28TRY-3A-2AGATHER-BACKTRACE-2A-20VARIABLE-29"></a>

- [variable] **\*GATHER-BACKTRACE\*** *T*

    Capturing the backtrace can be expensive. `*GATHER-BACKTRACE*`
    controls whether [`UNHANDLED-ERROR`][8f78]s shall have their [`BACKTRACE-OF`][3ace]
    populated.

<a id="x-28TRY-3ANLX-20CONDITION-29"></a>

- [condition] **NLX** *[ERROR\*][0321]*

    Representing a [non-local exit][b815] of unknown
    origin, this is signalled if a [`TRIAL`][99d0] does not return normally
    although it should have because it was not dismissed (see [`DISMISSAL`][0992],
    [`SKIP-TRIAL`][f45a], [`ABORT-TRIAL`][4f9f]). In this case, there is no `CL:ERROR`([`0`][d162] [`1`][35ba])
    associated with the event.

<a id="x-28TRY-3A-40CATEGORIES-20MGL-PAX-3ASECTION-29"></a>

### 4.8 Categories

Categories determine how event types are printed and events of
what types are counted together.

The default value of [`*CATEGORIES*`][e949] is

```
((abort*             :marker "⊟")
 (unexpected-failure :marker "⊠")
 (unexpected-success :marker "⊡")
 (skip               :marker "-")
 (expected-failure   :marker "×")
 (expected-success   :marker "⋅"))
```

which says that all concrete [`EVENT`][955d]s that are of type [`ABORT*`][8ec3] (i.e.
[`RESULT-ABORT*`][ffab], [`VERDICT-ABORT*`][4805], [`UNHANDLED-ERROR`][8f78], and [`NLX`][b115]) are to
be marked with `"⊟"` when printed (see [Printing Events][b3f9]). Also, the six
types define six counters for [Counting Events][886e]. Note that [`UNEXPECTED`][d6ad] events
have the same marker as their [`EXPECTED`][b194] counterpart but squared.

<a id="x-28TRY-3A-2ACATEGORIES-2A-20VARIABLE-29"></a>

- [variable] **\*CATEGORIES\*** *"- see above -"*

    A list of of elements like `(TYPE &KEY MARKER)`.
    When [Printing Events][b3f9], [Concrete Events][279a] are printed with the marker of the
    first matching type. When [Counting Events][886e], the counts associated with all
    matching types are incremented.

<a id="x-28TRY-3AFANCY-STD-CATEGORIES-20FUNCTION-29"></a>

- [function] **FANCY-STD-CATEGORIES**

    Returns the default value of [`*CATEGORIES*`][e949] (see [Categories][03ec]),
    which contains some fancy Unicode characters.

<a id="x-28TRY-3AASCII-STD-CATEGORIES-20FUNCTION-29"></a>

- [function] **ASCII-STD-CATEGORIES**

    Returns a value suitable for [`*CATEGORIES*`][e949], which uses only ASCII
    characters for the markers.
    
    ```
    '((abort*             :marker "!")
      (unexpected-failure :marker "F")
      (unexpected-success :marker ":")
      (skip               :marker "-")
      (expected-failure   :marker "f")
      (expected-success   :marker "."))
    ```


<a id="x-28TRY-3A-40IS-20MGL-PAX-3ASECTION-29"></a>

## 5 The `IS` Macro

[`IS`][80d6] is the fundamental one among [Checks][bb56], on which all
the others are built, and it is a replacement for [`CL:ASSERT`][97ee] that can
capture values of subforms to provide context to failures:

```common-lisp
(is (= (1+ 5) 0))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (= #1=(1+ 5) 0))
..   where
..     #1# = 6
```

`IS` automatically captures values of arguments to functions like [`1+`][1eb3]
in the above example. Values of other interesting subforms can be
explicitly requested to be captured. `IS` supports capturing multiple
values and can be taught how to deal with macros. The combination of
these features allows [`MATCH-VALUES`][162a] to be implementable as tiny
extension:

```common-lisp
(is (match-values (values (1+ 5) "sdf")
      (= * 0)
      (string= * "sdf")))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS
..      (MATCH-VALUES #1=(VALUES (1+ 5) #2="sdf")
..        (= * 0)
..        (STRING= * "sdf")))
..   where
..     #1# == 6
..            #2#
```

`IS` is flexible enough that all other checks ([`SIGNALS`][6d4e], [`SIGNALS-NOT`][7af9],
[`INVOKES-DEBUGGER`][12ce], [`INVOKES-DEBUGGER-NOT`][aaaa], [`FAILS`][e80e], and [`IN-TIME`][f3af] are built
on top of it.

<a id="x-28TRY-3AIS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **IS** *FORM &KEY MSG CTX (CAPTURE T) (PRINT-CAPTURES T) (RETRY T)*

    Evaluate `FORM` and signal a [`RESULT`][231f] [`SUCCESS`][269a] if its first return
    value is not `NIL`, else signal a `RESULT` [`FAILURE`][f92d] (see [Outcomes][e514]). `IS`
    returns normally if
    
    - the [`RECORD-EVENT`][ce49] restart is invoked (available when running in a
      trial), or
    
    - the [`CONTINUE`][1867] restart is invoked (available when not running in a
      trial), or
    
    - the signalled [`RESULT`][231f] condition is not handled (possible only when
      not running in a trial, and the result is a [`PASS`][21d9]).
    
    The return value of `IS` is `T` if the last condition signalled is a
    `SUCCESS`, and `NIL` otherwise.
    
    `MSG` and `CTX` are [Format Specifier Forms][3233]. `MSG` prints a description of
    the check being made, which is by default the whole `IS` form. Due to
    how conditions are printed, `MSG` says what the desired outcome is,
    and `CTX` provides information about the evaluation.
    
    ```common-lisp
    (is (equal (prin1-to-string 'hello) "hello")
        :msg "Symbols are replacements for strings." 
        :ctx ("*PACKAGE* is ~S and *PRINT-CASE* is ~S~%"
              *package* *print-case*))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     Symbols are replacements for strings.
    ..   where
    ..     (PRIN1-TO-STRING 'HELLO) = "HELLO"
    ..   *PACKAGE* is #<PACKAGE "TRY"> and *PRINT-CASE* is :UPCASE
    ..
    ```
    
    If `CAPTURE` is true, the value(s) of some subforms of `FORM` may be
    automatically recorded in the condition and also made available for
    `CTX` via [`*IS-CAPTURES*`][fb53]. See [Captures][3d27] for more.
    
    If `PRINT-CAPTURES` is true, the captures made are printed when the
    [`RESULT`][231f] condition is displayed in the debugger or [`*DESCRIBE*`][aa6d]d (see
    [Printing Events][b3f9]). This is the `where (PRIN1-TO-STRING 'HELLO) ="HELLO"`
    part above. If `PRINT-CAPTURES` is `NIL`, the captures are still
    available in `*IS-CAPTURES*` for writing custom `CTX` messages.
    
    If `RETRY` is true, then the [`RETRY-CHECK`][8cf6] restart evaluates `FORM` again
    and signals a new `RESULT`. If `RETRY` is `NIL`, then the `RETRY-CHECK`
    restart returns `:RETRY`, which allows complex checks such as [`SIGNALS`][6d4e]
    to implement their own retry mechanism.

<a id="x-28TRY-3A-2AIS-FORM-2A-20VARIABLE-29"></a>

- [variable] **\*IS-FORM\***

    [`IS`][80d6] binds this to its `FORM` argument for `CTX` and `MSG`.

<a id="x-28TRY-3A-2AIS-CAPTURES-2A-20VARIABLE-29"></a>

- [variable] **\*IS-CAPTURES\***

    Captures made during an [`IS`][80d6] evaluation are made available for
    `CTX` via `*IS-CAPTURES*`.

<a id="x-28TRY-3A-40FORMAT-SPECIFIER-FORMS-20MGL-PAX-3ASECTION-29"></a>

### 5.1 Format Specifier Forms

A format specifier form is a Lisp form, typically an argument to
macro, standing for the `FORMAT-CONTROL` and `FORMAT-ARGS` arguments to
the [`FORMAT`][ad78] function.

It may be a constant string:

```common-lisp
(is nil :msg "FORMAT-CONTROL~%with no args.")
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     FORMAT-CONTROL
..     with no args.
```

It may be a list whose first element is a constant string, and the
rest are the format arguments to be evaluated:

```common-lisp
(is nil :msg ("Implicit LIST ~A." "form"))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     Implicit LIST form.
```

Or it may be a form that evaluates to a list like `(FORMAT-CONTROL
&REST FORMAT-ARGS)`:

```common-lisp
(is nil :msg (list "Full ~A." "form"))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     Full form.
```

Finally, it may evaluate to `NIL`, in which case some context specific
default is implied.

<a id="x-28TRY-3ACANONICALIZE-FORMAT-SPECIFIER-FORM-20FUNCTION-29"></a>

- [function] **CANONICALIZE-FORMAT-SPECIFIER-FORM** *FORM*

    Ensure that the format specifier form `FORM` is in its full form.

<a id="x-28TRY-3A-40CAPTURES-20MGL-PAX-3ASECTION-29"></a>

### 5.2 Captures

During the evaluation of the `FORM` argument of [`IS`][80d6], evaluation of any
form (e.g. a subform of `FORM`) may be recorded, which are called
captures.

<a id="x-28TRY-3A-40AUTOMATIC-CAPTURES-20MGL-PAX-3ASECTION-29"></a>

#### 5.2.1 Automatic Captures

[`IS`][80d6] automatically captures some subforms of `FORM` that are likely
to be informative. In particular, if `FORM` is a function call, then
non-constant arguments are automatically captured:

```common-lisp
(is (= 3 (1+ 2) (- 4 3)))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (= 3 #1=(1+ 2) #2=(- 4 3)))
..   where
..     #1# = 3
..     #2# = 1
```

By default, automatic captures are not made for subforms deeper in
`FORM`, except for when `FORM` is a call to [`NULL`][25f5],
[`ENDP`][e8d7] and [`NOT`][1013]:

```common-lisp
(is (null (find (1+ 1) '(1 2 3))))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (NULL #1=(FIND #2=(1+ 1) '(1 2 3))))
..   where
..     #2# = 2
..     #1# = 2
```

```common-lisp
(is (endp (member (1+ 1) '(1 2 3))))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (ENDP #1=(MEMBER #2=(1+ 1) '(1 2 3))))
..   where
..     #2# = 2
..     #1# = (2 3)
```

Note that the argument of [`NOT`][1013] is not captured as it is
assumed to be `NIL` or `T`. If that's not true, use [`NULL`][25f5].

```common-lisp
(is (not (equal (1+ 5) 6)))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (NOT (EQUAL #1=(1+ 5) 6)))
..   where
..     #1# = 6
```

Other automatic captures are discussed with the relevant
functionality such as [`MATCH-VALUES`][162a].

<a id="x-28TRY-3A-40WRITING-AUTOMATIC-CAPTURE-RULES-20MGL-PAX-3ASECTION-29"></a>

##### 5.2.1.1 Writing Automatic Capture Rules

<a id="x-28TRY-3ASUB-20CLASS-29"></a>

- [class] **SUB** *[STRUCTURE-OBJECT][2038]*

    A `SUB` (short for substitution) says that in the original form [`IS`][80d6] is
    checking, a `SUBFORM` was substituted (by `SUBSTITUTE-IS-FORM`) with
    `VAR` (if `VALUESP` is `NIL`) or with ([`VALUES-LIST`][dbd4] `VAR`) if `VALUESP` is
    true. Conversely, `VAR` is to be bound to the evaluated `NEW-FORM` if
    `VALUESP` is `NIL`, and to ([`MULTIPLE-VALUE-LIST`][4444] `FORM`) if `VALUESP`.
    `NEW-FORM` is often [`EQ`][5a82] to `SUBFORM`, but it may be different, which is
    the case when further substitutions are made within a substitution.

<a id="x-28TRY-3AMAKE-SUB-20FUNCTION-29"></a>

- [function] **MAKE-SUB** *VAR SUBFORM NEW-FORM VALUESP*

<a id="x-28TRY-3ASUB-VAR-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29"></a>

- [structure-accessor] **SUB-VAR** *SUB*

<a id="x-28TRY-3ASUB-SUBFORM-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29"></a>

- [structure-accessor] **SUB-SUBFORM** *SUB*

<a id="x-28TRY-3ASUB-NEW-FORM-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29"></a>

- [structure-accessor] **SUB-NEW-FORM** *SUB*

<a id="x-28TRY-3ASUB-VALUESP-20-28MGL-PAX-3ASTRUCTURE-ACCESSOR-20TRY-3ASUB-29-29"></a>

- [structure-accessor] **SUB-VALUESP** *SUB*

<a id="x-28TRY-3ASUBSTITUTE-IS-LIST-FORM-20GENERIC-FUNCTION-29"></a>

- [generic-function] **SUBSTITUTE-IS-LIST-FORM** *FIRST FORM ENV*

    In the list `FORM`, whose [`CAR`][d5a2] is `FIRST`, substitute
    subexpressions of interest with a [`GENSYM`][0e59] and return the new form. As
    the second value, return a list of [`SUB`][6b0e]s.
    
    For example, consider `(IS (FIND (FOO) LIST))`. When
    `SUBSTITUTE-IS-LIST-FORM` is invoked on `(FIND (FOO) LIST)`, it
    substitutes each argument of [`FIND`][4e46] with a variable, returning the new
    form `(FIND TEMP1 TEMP2)` and the list of two
    substitutions `((TEMP2 (FOO) (FOO) NIL) (TEMP3 LIST LIST NIL))`.
    This allows the original form to be rewritten as
    
    ```
    (let* ((temp1 (foo))
           (temp2 list))
      (find temp1 temp2))
    ```
    
    `TEMP1` and `TEMP2` may then be reported in the [`OUTCOME`][2656] condition
    signalled by [`IS`][80d6] like this:
    
        The following check failed:
          (is (find #1=(foo) #2=list))
        where
          #1# = <return-value-of-foo>
          #2# = <value-of-variable-list>


<a id="x-28TRY-3A-40EXPLICIT-CAPTURES-20MGL-PAX-3ASECTION-29"></a>

#### 5.2.2 Explicit Captures

In addition to automatic captures, which are prescribed by
rewriting rules (see [Writing Automatic Capture Rules][0743]), explicit,
ad-hoc captures can also be made.

```common-lisp
(is (let ((x 1))
      (= (capture x) 2)))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS
..      (LET ((X 1))
..        (= (CAPTURE X) 2)))
..   where
..     X = 1
```

If [`CAPTURE`][19f3] showing up in the form that [`IS`][80d6] prints is undesirable,
then [`%`][790c] may be used instead:

```common-lisp
(is (let ((x 1))
      (= (% x) 2)))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS
..      (LET ((X 1))
..        (= X 2)))
..   where
..     X = 1
```

Multiple values may be captured with [`CAPTURE-VALUES`][351f] and its
secretive counterpart [`%%`][c1f6]:

```common-lisp
(is (= (%% (values 1 2)) 2))
.. debugger invoked on UNEXPECTED-RESULT-FAILURE:
..   UNEXPECTED-FAILURE in check:
..     (IS (= #1=(VALUES 1 2) 2))
..   where
..     #1# == 1
..            2
```

where printing `==` instead of = indicates that this
is a multiple value capture.

<a id="x-28TRY-3ACAPTURE-20MGL-PAX-3AMACRO-29"></a>

- [macro] **CAPTURE** *FORM*

    Evaluate `FORM`, record its primary return value if within the
    dynamic extent of an [`IS`][80d6] evaluation, and finally return that value.
    If `CAPTURE` is used within the lexical scope of `IS`, then `CAPTURE`
    itself will show up in the form that the default `MSG` prints. Thus it
    is recommended to use the equivalent [`MACROLET`][1383] [`%`][790c] in the lexical
    scope as `%` is removed before printing.

<a id="x-28TRY-3ACAPTURE-VALUES-20MGL-PAX-3AMACRO-29"></a>

- [macro] **CAPTURE-VALUES** *FORM*

    Like `CAPTURE-VALUES`, but record and return all values returned by
    `FORM`. It is recommended to use the equivalent [`MACROLET`][1383] [`%%`][c1f6] in the
    lexical scope as `%%` is removed before printing.

<a id="x-28TRY-3A-25-20MACROLET-29"></a>

- [macrolet] **%** *FORM*

    An alias for [`CAPTURE`][19f3] in the lexical scope of [`IS`][80d6]. Removed from the
    `IS` form when printed.

<a id="x-28TRY-3A-25-25-20MACROLET-29"></a>

- [macrolet] **%%** *FORM*

    An alias for [`CAPTURE-VALUES`][351f] in the lexical scope of [`IS`][80d6]. Removed
    from the `IS` form when printed.

<a id="x-28TRY-3A-40CHECK-LIBRARY-20MGL-PAX-3ASECTION-29"></a>

## 6 Check Library

In the following, various checks built on top of [`IS`][80d6] are described.
Many of them share a number of arguments, which are described here.

- `ON-RETURN` is a boolean that determines whether the check in a
  macro that wraps `BODY` is made when `BODY` returns normally.

- `ON-NLX` is a boolean that determines whether the check in a macro
  that wraps `BODY` is made when `BODY` performs a [non-local exit][b815].

- `MSG` and `CTX` are [Format Specifier Forms][3233] as in `IS`.

- `NAME` may be provided so that it is printed (with [`PRIN1`][6384]) instead of
  `BODY` in `MSG`.


<a id="x-28TRY-3A-40CHECKING-CONDITIONS-20MGL-PAX-3ASECTION-29"></a>

### 6.1 Checking Conditions

The macros [`SIGNALS`][6d4e], [`SIGNALS-NOT`][7af9], [`INVOKES-DEBUGGER`][12ce], and
[`INVOKES-DEBUGGER-NOT`][aaaa] all check whether a condition of a given type,
possibly also matching a predicate, was signalled. In addition to
those already described in [Check Library][f7f7], these macros share a
number of arguments.

Matching conditions are those that are of type `CONDITION-TYPE` (not
evaluated) and satisfy the predicate `PRED`.

When `PRED` is `NIL`, it always matches. When it is a string, then it
matches if it is a substring of the printed representation of the
condition being handled (by [`PRINC`][676d] under [`WITH-STANDARD-IO-SYNTAX`][39df]).
When it is a function, it matches if it returns true when called
with the condition as its argument.

The check is performed in the cleanup form of an [`UNWIND-PROTECT`][c93f]
around `BODY`. If the [`CURRENT-TRIAL`][e186] is performing an [`ABORT-TRIAL`][4f9f],
[`SKIP-TRIAL`][f45a] or [`RETRY-TRIAL`][fae3], then [`RESULT-SKIP`][7c3f] is signalled.

`HANDLER` is called when a matching condition is found. It can be a
function, `T`, or `NIL`. When it is a function, it is called from the
condition handler (`SIGNALS` and `SIGNALS-NOT`) or the debugger
hook (`INVOKES-DEBUGGER` and `INVOKES-DEBUGGER-NOT`) with the matching
condition. `HANDLER` may perform a [non-local exit][b815]. When
`HANDLER` is `T`, the matching condition is handled by performing a
non-local exit to just outside `BODY`. If the exit completes, `BODY` is
treated as if it had returned normally, and `ON-RETURN` is consulted.
When `HANDLER` is `NIL`, no addition action is performed when a matching
condition is found.

The default `CTX` describes the result of the matching process in
terms of [`*CONDITION-MATCHED-P*`][cf88] and [`*BEST-MATCHING-CONDITION*`][a07f].

<a id="x-28TRY-3A-2ACONDITION-MATCHED-P-2A-20VARIABLE-29"></a>

- [variable] **\*CONDITION-MATCHED-P\***

    When a check described in [Checking Conditions][ff2c] signals its
    [`OUTCOME`][2656], this variable is bound to a boolean value to indicate
    whether a condition that matched `CONDITION-TYPE` and `PRED` was
    found.

<a id="x-28TRY-3A-2ABEST-MATCHING-CONDITION-2A-20VARIABLE-29"></a>

- [variable] **\*BEST-MATCHING-CONDITION\***

    Bound when a check described in [Checking Conditions][ff2c]
    signals its [`OUTCOME`][2656]. If [`*CONDITION-MATCHED-P*`][cf88], then it is the
    most recent condition that matched both `CONDITION-TYPE` and `PRED`.
    Else, it is the most recent condition that matched
    `CONDITION-TYPE` or `NIL` if no such conditions were detected.

<a id="x-28TRY-3ASIGNALS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **SIGNALS** *(CONDITION-TYPE &KEY PRED (HANDLER T) (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` signals a [`CONDITION`][83e1] of `CONDITION-TYPE` (not
    evaluated) that matches `PRED`. To detect matching conditions, `SIGNALS`
    sets up a [`HANDLER-BIND`][fd3c]. Thus it can only see what `BODY` does not
    handle. The arguments are described in [Checking Conditions][ff2c].
    
    ```common-lisp
    (signals (error)
      (error "xxx"))
    => NIL
    ```
    
    The following example shows a failure where `CONDITION-TYPE` matches
    but `PRED` does not.
    
    ```common-lisp
    (signals (error :pred "non-matching")
      (error "xxx"))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (ERROR "xxx") signals a condition of type ERROR that matches
    ..     "non-matching".
    ..   The predicate did not match "xxx".
    ```


<a id="x-28TRY-3ASIGNALS-NOT-20MGL-PAX-3AMACRO-29"></a>

- [macro] **SIGNALS-NOT** *(CONDITION-TYPE &KEY PRED (HANDLER T) (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` does not signal a [`CONDITION`][83e1] of `CONDITION-TYPE` (not
    evaluated) that matches `PRED`. To detect matching conditions,
    `SIGNALS-NOT` sets up a [`HANDLER-BIND`][fd3c]. Thus, it can only see what `BODY`
    does not handle. The arguments are described in
    [Checking Conditions][ff2c].

<a id="x-28TRY-3AINVOKES-DEBUGGER-20MGL-PAX-3AMACRO-29"></a>

- [macro] **INVOKES-DEBUGGER** *(CONDITION-TYPE &KEY PRED (HANDLER T) (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` enters the debugger with a [`CONDITION`][83e1] of
    `CONDITION-TYPE` (not evaluated) that matches `PRED`. To detect matching
    conditions, `INVOKES-DEBUGGER` sets up a [`*DEBUGGER-HOOK*`][1cdc]. Thus, if
    `*DEBUGGER-HOOK*` is changed by `BODY`, it may not detect the condition.
    The arguments are described in [Checking Conditions][ff2c].
    
    Note that in a trial (see [`CURRENT-TRIAL`][e186]), all `ERROR`([`0`][d162] [`1`][35ba])s are handled,
    and a `*DEBUGGER-HOOK*` is set up (see [`UNHANDLED-ERROR`][8f78]). Thus,
    invoking debugger would normally cause the trial to abort.
    
    ```common-lisp
    (invokes-debugger (error :pred "xxx")
      (handler-bind ((error #'invoke-debugger))
        (error "xxx")))
    => NIL
    ```


<a id="x-28TRY-3AINVOKES-DEBUGGER-NOT-20MGL-PAX-3AMACRO-29"></a>

- [macro] **INVOKES-DEBUGGER-NOT** *(CONDITION-TYPE &KEY PRED (HANDLER T) (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` does not enter the debugger with a [`CONDITION`][83e1] of
    `CONDITION-TYPE` (not evaluated) that matches `PRED`. To detect matching
    conditions, `INVOKES-DEBUGGER-NOT` sets up a [`*DEBUGGER-HOOK*`][1cdc]. Thus, if
    `*DEBUGGER-HOOK*` is changed by `BODY`, it may not detect the condition.
    The arguments are described in [Checking Conditions][ff2c].

<a id="x-28TRY-3A-40MISC-CHECKS-20MGL-PAX-3ASECTION-29"></a>

### 6.2 Miscellaneous Checks

<a id="x-28TRY-3AFAILS-20MGL-PAX-3AMACRO-29"></a>

- [macro] **FAILS** *(&KEY NAME MSG CTX) &BODY BODY*

    Check that `BODY` performs a [non-local exit][b815] but do not
    cancel it (see [cancelled non-local exit][7ab6]). See [Check Library][f7f7] for the
    descriptions of the other arguments.
    
    In the following example, `FAILS` signals a [`SUCCESS`][269a].
    
    ```common-lisp
    (catch 'foo
      (fails ()
        (throw 'foo 7)))
    => 7
    ```
    
    Next, `FAILS` signals an [`UNEXPECTED-FAILURE`][b5cb] because `BODY` returns
    normally.
    
    ```common-lisp
    (fails ()
      (print 'hey))
    ..
    .. HEY 
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (PRINT 'HEY) does not return normally.
    ```
    
    Note that there is no `FAILS-NOT` as [`WITH-TEST`][8f5d] fills that role.

<a id="x-28TRY-3AIN-TIME-20MGL-PAX-3AMACRO-29"></a>

- [macro] **IN-TIME** *(SECONDS &KEY (ON-RETURN T) (ON-NLX T) NAME MSG CTX) &BODY BODY*

    Check that `BODY` finishes in `SECONDS`. See [Check Library][f7f7] for
    the descriptions of the other arguments.
    
    ```
    (in-time (1)
      (sleep 2))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (SLEEP 2) finishes within 1s.
    ..   Took 2.000s.
    ```
    
    [`RETRY-CHECK`][8cf6] restarts timing.

<a id="x-28TRY-3A-2AIN-TIME-ELAPSED-SECONDS-2A-20VARIABLE-29"></a>

- [variable] **\*IN-TIME-ELAPSED-SECONDS\***

    Bound to the number of seconds passed during the evaluation of
    `BODY` when [`IN-TIME`][f3af] signals its [`OUTCOME`][2656].

<a id="x-28TRY-3A-40CHECK-UTILITIES-20MGL-PAX-3ASECTION-29"></a>

### 6.3 Check Utilities

These utilities are not checks (which signal [`OUTCOME`][2656]s) but simple
functions and macros that may be useful for writing [`IS`][80d6] checks.

<a id="x-28TRY-3AON-VALUES-20MGL-PAX-3AMACRO-29"></a>

- [macro] **ON-VALUES** *FORM &BODY BODY*

    `ON-VALUES` evaluates `FORM` and transforms its return values one by
    one based on forms in `BODY`. The Nth value is replaced by the return
    value of the Nth form of `BODY` evaluated with `*` bound
    to the Nth value. If the number of values exceeds the number of
    transformation forms in `BODY` then the excess values are returned as
    is.
    
    ```common-lisp
    (on-values (values 1 "abc" 7)
      (1+ *)
      (length *))
    => 2
    => 3
    => 7
    ```
    
    If the number of values is less than the number of transformation
    forms, then in later transformation forms `*` is bound
    to `NIL`.
    
    ```common-lisp
    (on-values (values)
      *
      *)
    => NIL
    => NIL
    ```
    
    The first forms in `BODY` may be options. Options must precede
    transformation forms. With `:TRUNCATE` `T`, the excess values are
    discarded.
    
    ```common-lisp
    (on-values (values 1 "abc" 7)
      (:truncate t)
      (1+ *)
      (length *))
    => 2
    => 3
    ```
    
    The `:ON-LENGTH-MISMATCH` option may be `NIL` or a function of a single
    argument. If the number of values and the number of transformation
    forms are different, then this function is called to transform the
    list of values. `:TRUNCATE` is handled before `:ON-LENGTH-MISMATCH`.
    
    ```common-lisp
    (on-values 1
      (:on-length-mismatch (lambda (values)
                             (if (= (length values) 1)
                                 (append values '("abc"))
                                 values)))
      (1+ *)
      *)
    => 2
    => "abc"
    ```
    
    If the same option is specified multiple times, the first one is in
    effect.

<a id="x-28TRY-3AMATCH-VALUES-20MGL-PAX-3AMACRO-29"></a>

- [macro] **MATCH-VALUES** *FORM &BODY BODY*

    `MATCH-VALUES` returns true iff all return values of `FORM` satisfy
    the predicates given by `BODY`, which are described in [`ON-VALUES`][eb5c]. The
    `:ON-LENGTH-MISMATCH` and `:TRUNCATE` options of `ON-VALUES` are
    supported. If no `:ON-LENGTH-MISMATCH` option is specified, then
    `MATCH-VALUES` returns `NIL` on length mismatch.
    
    ```common-lisp
    ;; no values
    (is (match-values (values)))
    ;; single value success
    (is (match-values 1
          (= * 1)))
    ;; success with different types
    (is (match-values (values 1 "sdf")
          (= * 1)
          (string= * "sdf")))
    ;; too few values
    (is (not (match-values 1
               (= * 1)
               (string= * "sdf"))))
    ;; too many values
    (is (not (match-values (values 1 "sdf" 3)
               (= * 1)
               (string= * "sdf"))))
    ;; too many values, but truncated
    (is (match-values (values 1 "sdf" 3)
          (:truncate t)
          (= * 1)
          (string= * "sdf")))
    ```


<a id="x-28TRY-3AMISMATCH-25-20FUNCTION-29"></a>

- [function] **MISMATCH%** *SEQUENCE1 SEQUENCE2 &KEY FROM-END (TEST \#'EQL) (START1 0) END1 (START2 0) END2 KEY MAX-PREFIX-LENGTH MAX-SUFFIX-LENGTH*

    Like [`CL:MISMATCH`][b94a] but [`CAPTURE`][19f3]s and returns the common prefix and
    the mismatched suffixes. The `TEST-NOT` argument is deprecated by
    the `CLHS` and is not supported. In addition, if `MAX-PREFIX-LENGTH` and
    `MAX-SUFFIX-LENGTH` are non-`NIL`, they must be non-negative integers,
    and they limit the number of elements in the prefix and the
    suffixes.
    
    ```common-lisp
    (is (null (mismatch% '(1 2 3) '(1 2 4 5))))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (IS (NULL #1=(MISMATCH% '(1 2 3) '(1 2 4 5))))
    ..   where
    ..     COMMON-PREFIX = (1 2)
    ..     MISMATCHED-SUFFIX-1 = (3)
    ..     MISMATCHED-SUFFIX-2 = (4 5)
    ..     #1# = 2
    ```
    
    ```common-lisp
    (is (null (mismatch% "Hello, World!"
                         "Hello, world!")))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (IS (NULL #1=(MISMATCH% "Hello, World!" "Hello, world!")))
    ..   where
    ..     COMMON-PREFIX = "Hello, "
    ..     MISMATCHED-SUFFIX-1 = "World!"
    ..     MISMATCHED-SUFFIX-2 = "world!"
    ..     #1# = 7
    ```


<a id="x-28TRY-3ADIFFERENT-ELEMENTS-20FUNCTION-29"></a>

- [function] **DIFFERENT-ELEMENTS** *SEQUENCE1 SEQUENCE2 &KEY (PRED \#'EQL) (MISSING :MISSING)*

    Return the different elements under `PRED` in the given sequences as
    a list of `(:INDEX <INDEX> <E1> <E2>)` elements, where `E1` and `E2`
    are elements of `SEQUENCE1` and `SEQUENCE2` at `<INDEX>`, respectively,
    and they may be `MISSING` if the corresponding sequence is too short.
    
    ```common-lisp
    (is (endp (different-elements '(1 2 3) '(1 b 3 d))))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (IS (ENDP #1=(DIFFERENT-ELEMENTS '(1 2 3) '(1 B 3 D))))
    ..   where
    ..     #1# = ((:INDEX 1 2 B) (:INDEX 3 :MISSING D))
    ```


<a id="x-28TRY-3ASAME-SET-P-20FUNCTION-29"></a>

- [function] **SAME-SET-P** *LIST1 LIST2 &KEY KEY (TEST \#'EQL)*

    See if `LIST1` and `LIST2` represent the same set.
    See [`CL:SET-DIFFERENCE`][f8bf] for a description of the `KEY` and `TEST` arguments.
    
    ```common-lisp
    (try:is (try:same-set-p '(1) '(2)))
    .. debugger invoked on UNEXPECTED-RESULT-FAILURE:
    ..   UNEXPECTED-FAILURE in check:
    ..     (IS (SAME-SET-P '(1) '(2)))
    ..   where
    ..     ONLY-IN-1 = (1)
    ..     ONLY-IN-2 = (2)
    ```


<a id="x-28TRY-3AWITH-SHUFFLING-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-SHUFFLING** *NIL &BODY BODY*

    Execute the forms that make up the list of forms `BODY` in random
    order and return `NIL`. This may be useful to prevent writing tests
    that accidentally depend on the order in which subtests are called.
    
    ```common-lisp
    (loop repeat 3 do
      (with-shuffling ()
        (prin1 1)
        (prin1 2)))
    .. 122112
    => NIL
    ```


<a id="x-28TRY-3A-40COMPARING-FLOATS-20MGL-PAX-3ASECTION-29"></a>

#### 6.3.1 Comparing Floats

Float comparisons following
[https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/](https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/).

<a id="x-28TRY-3AFLOAT--7E-3D-20FUNCTION-29"></a>

- [function] **FLOAT-~=** *X Y &KEY (MAX-DIFF-IN-VALUE \*MAX-DIFF-IN-VALUE\*) (MAX-DIFF-IN-ULP \*MAX-DIFF-IN-ULP\*)*

    Return whether two numbers, `X` and `Y`, are approximately equal either
    according to `MAX-DIFF-IN-VALUE` or `MAX-DIFF-IN-ULP`.
    
    If the absolute value of the difference of two floats is not greater
    than `MAX-DIFF-IN-VALUE`, then they are considered equal.
    
    If two floats are of the same sign and the number of representable
    floats (ULP, unit in the last place) between them is less than
    `MAX-DIFF-IN-ULP`, then they are considered equal.
    
    If neither `X` nor `Y` are floats, then the comparison is done with [`=`][e52f].
    If one of them is a [`DOUBLE-FLOAT`][0d57], then the other is converted to a
    double float, and the comparison takes place in double float space.
    Else, both are converted to [`SINGLE-FLOAT`][31a6] and the comparison takes
    place in single float space.

<a id="x-28TRY-3A-2AMAX-DIFF-IN-VALUE-2A-20VARIABLE-29"></a>

- [variable] **\*MAX-DIFF-IN-VALUE\*** *1.0e-16*

    The default value of the `MAX-DIFF-IN-VALUE` argument of [`FLOAT-~=`][7955].

<a id="x-28TRY-3A-2AMAX-DIFF-IN-ULP-2A-20VARIABLE-29"></a>

- [variable] **\*MAX-DIFF-IN-ULP\*** *2*

    The default value of the `MAX-DIFF-IN-ULP` argument of [`FLOAT-~=`][7955].

<a id="x-28TRY-3AFLOAT--7E-3C-20FUNCTION-29"></a>

- [function] **FLOAT-~\<** *X Y &KEY (MAX-DIFF-IN-VALUE \*MAX-DIFF-IN-VALUE\*) (MAX-DIFF-IN-ULP \*MAX-DIFF-IN-ULP\*)*

    Return whether `X` is approximately less than `Y`. Equivalent to [`<`][c3a0],
    but it also allows for approximate equality according to [`FLOAT-~=`][7955].

<a id="x-28TRY-3AFLOAT--7E-3E-20FUNCTION-29"></a>

- [function] **FLOAT-~>** *X Y &KEY (MAX-DIFF-IN-VALUE \*MAX-DIFF-IN-VALUE\*) (MAX-DIFF-IN-ULP \*MAX-DIFF-IN-ULP\*)*

    Return whether `X` is approximately greater than `Y`. Equivalent to [`>`][5333],
    but it also allows for approximate equality according to [`FLOAT-~=`][7955].

<a id="x-28TRY-3A-40TESTS-20MGL-PAX-3ASECTION-29"></a>

## 7 Tests

In Try, tests are Lisp functions that record their execution in
[`TRIAL`][99d0] objects. `TRIAL`s are to tests what function call traces are to
functions. In more detail, tests

- create a `TRIAL` object and signal a [`TRIAL-START`][b664] event upon entry to
  the function,

- signal a [`VERDICT`][52e1] condition before returning normally or via a
  [non-local exit][b815],

- return the `TRIAL` object as the first value,

- return explicitly returned values as the second, third, and so on
  values.

See [`DEFTEST`][e7ca] and [`WITH-TEST`][8f5d] for more precise descriptions.

<a id="x-28TRY-3ADEFTEST-20MGL-PAX-3AMACRO-29"></a>

- [macro] **DEFTEST** *NAME LAMBDA-LIST &BODY BODY*

    `DEFTEST` is a wrapper around [`DEFUN`][f472] to define global test functions.
    See `DEFUN` for a description of `NAME`, `LAMBDA-LIST`, and `BODY`. The
    behaviour common with [`WITH-TEST`][8f5d] is described in [Tests][dc28].
    
    ```common-lisp
    (deftest my-test ()
      (write-string "hey"))
    => MY-TEST
    
    (test-bound-p 'my-test)
    => T
    
    (my-test)
    .. hey
    ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s>
    ```
    
    Although the common case is for tests to have no arguments, `DEFTEST`
    supports general function lambda lists. Within a global test,
    
    - `NAME` is bound to the [`TRIAL`][99d0] object
    
    - the first return value is the trial
    
    - values are not returned implicitly
    
    - values returned with an explicit [`RETURN-FROM`][3eef7] are returned as
      values after the trial
    
    ```common-lisp
    (deftest my-test ()
      (prin1 my-test)
      (return-from my-test (values 2 3)))
    
    (my-test)
    .. #<TRIAL (MY-TEST) RUNNING>
    ==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s>
    => 2
    => 3
    ```


<a id="x-28TRY-3A-2ARUN-DEFTEST-WHEN-2A-20VARIABLE-29"></a>

- [variable] **\*RUN-DEFTEST-WHEN\*** *NIL*

    This may be any of `:COMPILE-TOPLEVEL`, `:LOAD-TOPLEVEL`, `:EXECUTE`, or
    a list thereof. The value of `*RUN-DEFTEST-WHEN*` determines in what
    [`EVAL-WHEN`][9c9c] situation to call the test function immediately after it
    has been defined with [`DEFTEST`][e7ca].
    
    For interactive development, it may be convenient to set it to
    `:EXECUTE` and have the test run when the `DEFTEST` is evaluated (maybe
    with Slime `C-M-x`, `slime-eval-defun`). Or set it to
    `:COMPILE-TOPLEVEL`, and have it rerun on Slime `C-c C-c`,
    `slime-compile-defun`.
    
    If the test has required arguments, an argument list is prompted for
    and read from [`*QUERY-IO*`][f4bf].

<a id="x-28TRY-3ATEST-BOUND-P-20FUNCTION-29"></a>

- [function] **TEST-BOUND-P** *SYMBOL*

    See if `SYMBOL` names a global test (i.e. a test defined with
    [`DEFTEST`][e7ca]). If since the execution of `DEFTEST`, the symbol has been
    [`UNINTERN`][cdba]ed, [`FMAKUNBOUND`][609c]ed, or redefined with [`DEFUN`][f472], then it no
    longer names a global test.

<a id="x-28TRY-3AWITH-TEST-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-TEST** *(&OPTIONAL TRIAL-VAR &KEY NAME) &BODY BODY*

    Define a so-called lambda test to group together `CHECK`s and other
    tests it executes. `WITH-TEST` executes `BODY` in its lexical
    environment even on a rerun (see [Rerunning Trials][e4ac]).
    
    If `TRIAL-VAR` is a non-`NIL` symbol, bind it to the trial object.
    `NAME` may be any type, it is purely for presentation purposes. If
    `NAME` is `NIL`, then it defaults to `TRIAL-VAR`.
    
    To facilitate returning values, a [`BLOCK`][d2d8] is wrapped around `BODY`. The
    name of the block is `TRIAL-VAR` if it is a symbol, else it's `NIL`.
    
    When both `TRIAL-VAR` and `NAME` are specified:
    
    ```common-lisp
    (with-test (some-feature :name "obscure feature")
      (prin1 some-feature)
      (is t)
      (return-from some-feature (values 1 2)))
    .. #<TRIAL (WITH-TEST ("obscure feature")) RUNNING>
    .. obscure feature
    ..   ⋅ (IS T)
    .. ⋅ obscure feature ⋅1
    ..
    ==> #<TRIAL (WITH-TEST ("obscure feature")) EXPECTED-SUCCESS 0.002s ⋅1>
    => 1
    => 2
    ```
    
    If only `TRIAL-VAR` is specified:
    
    ```common-lisp
    (with-test (some-feature)
      (prin1 some-feature)
      (is t)
      (return-from some-feature (values 1 2)))
    .. #<TRIAL (WITH-TEST (SOME-FEATURE)) RUNNING>
    .. SOME-FEATURE
    ..   ⋅ (IS T)
    .. ⋅ SOME-FEATURE ⋅1
    ..
    ==> #<TRIAL (WITH-TEST (SOME-FEATURE)) EXPECTED-SUCCESS 0.000s ⋅1>
    => 1
    => 2
    ```
    
    If neither is specified:
    
    ```common-lisp
    (with-test ()
      (prin1 (current-trial))
      (is t)
      (return (values 1 2)))
    .. #<TRIAL (WITH-TEST (NIL)) RUNNING>
    .. NIL
    ..   ⋅ (IS T)
    .. ⋅ NIL ⋅1
    ..
    ==> #<TRIAL (WITH-TEST (NIL)) EXPECTED-SUCCESS 0.000s ⋅1>
    => 1
    => 2
    ```
    
    Finally, using that `NAME` defaults to `TRIAL-VAR` and that it is valid
    to specify non-symbols for `TRIAL-VAR`, one can also write:
    
    ```common-lisp
    (with-test ("Some feature")
      (prin1 (current-trial))
      (is t)
      (return (values 1 2)))
    .. #<TRIAL (WITH-TEST ("Some feature")) RUNNING>
    .. Some feature
    ..   ⋅ (IS T)
    .. ⋅ Some feature ⋅1
    ..
    ==> #<TRIAL (WITH-TEST ("Some feature")) EXPECTED-SUCCESS 0.000s ⋅1>
    => 1
    => 2
    ```
    
    In summary and in contrast to global tests (those defined with
    [`DEFTEST`][e7ca]), lambda tests
    
    - have no arguments,
    
    - are defined and called at the same time,
    
    - may not bind their trial object to any variable,
    
    - may have a `BLOCK` named `NIL`,
    
    - have a `NAME` purely for presentation purposes.
    
    Lambda tests can be thought of as analogous to `(FUNCALL (LAMBDA ()
    BODY))`. The presence of the `LAMBDA`([`0`][e400] [`1`][5c01]) is important because it is
    stored in the [`TRIAL`][99d0] object to support [Rerunning Trials][e4ac].

<a id="x-28TRY-3ALIST-PACKAGE-TESTS-20FUNCTION-29"></a>

- [function] **LIST-PACKAGE-TESTS** *&OPTIONAL (PACKAGE \*PACKAGE\*)*

    List all symbols in `PACKAGE` that name global tests in the sense of
    [`TEST-BOUND-P`][5065].

<a id="x-28TRY-3AWITH-TESTS-RUN-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WITH-TESTS-RUN** *(TESTS-RUN) &BODY BODY*

    Bind the symbol `TESTS-RUN` to an empty [`EQ`][5a82] hash table and execute
    `BODY`. The has table reflects call counts to global tests. Keys are
    symbols naming global tests, and the values are the number of times
    the keys have been called.

<a id="x-28TRY-3AWARN-ON-TESTS-NOT-RUN-20MGL-PAX-3AMACRO-29"></a>

- [macro] **WARN-ON-TESTS-NOT-RUN** *(&OPTIONAL (PACKAGE \*PACKAGE\*)) &BODY BODY*

    A convenience utility to that records the global tests run by `BODY`
    with [`WITH-TESTS-RUN`][6910] and, when `BODY` finishes, signals a warning for
    each global tests in `PACKAGE` not run.
    
    This is how Try runs its own tests:
    
    ```
    (defun test ()
      ;; Bind *PACKAGE* so that names of tests printed have package names,
      ;; and M-. works on them in Slime.
      (let ((*package* (find-package :common-lisp)))
        (warn-on-tests-not-run ((find-package :try))
          (print (try 'test-all
                      :print 'unexpected
                      :describe 'unexpected)))))
    ```


<a id="x-28TRY-3A-40IMPLICIT-TRY-20MGL-PAX-3ASECTION-29"></a>

### 7.1 Calling Test Functions

Tests can be run explicitly by invoking the [`TRY`][b602] function or
implicitly by calling a test function:

```common-lisp
(deftest my-test ()
  (is t))

(my-test)
.. MY-TEST
..   ⋅ (IS T)
.. ⋅ MY-TEST ⋅1
..
==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.004s ⋅1>
```

The situation is similar with a [`WITH-TEST`][8f5d]:

```common-lisp
(with-test (my-test)
  (is t))
.. MY-TEST
..   ⋅ (IS T)
.. ⋅ MY-TEST ⋅1
..
==> #<TRIAL (WITH-TEST (MY-TEST)) EXPECTED-SUCCESS 0.000s ⋅1>
```

Behind the scenes, the outermost test function calls `TRY` with

```
(try trial :debug *debug* :collect *collect* :rerun *rerun*
     :print *print* :describe *describe*
     :stream *stream* :printer *printer*)
```

`TRY` then calls the test function belonging to `TRIAL`.
The rest of the behaviour is described in [Explicit `TRY`][1720].

<a id="x-28TRY-3A-2ADEBUG-2A-20VARIABLE-29"></a>

- [variable] **\*DEBUG\*** *(AND UNEXPECTED (NOT NLX) (NOT VERDICT))*

    The default value makes [`TRY`][b602] invoke the debugger on [`UNHANDLED-ERROR`][8f78],
    [`RESULT-ABORT*`][ffab], [`UNEXPECTED-RESULT-FAILURE`][daeb], and
    [`UNEXPECTED-RESULT-SUCCESS`][b72c]. [`NLX`][b115] is excluded because it is caught as
    the test function is being exited, but by that time the dynamic
    environment of the actual cause is likely gone. [`VERDICT`][52e1] is excluded
    because it is a consequence of its child outcomes.

<a id="x-28TRY-3A-2ACOUNT-2A-20VARIABLE-29"></a>

- [variable] **\*COUNT\*** *LEAF*

    Although the default value of [`*CATEGORIES*`][e949] lumps [`RESULT`][231f]s and
    [`VERDICT`][52e1]s together, with the default of [`LEAF`][f58d], `VERDICT`s are not
    counted. See [Counting Events][886e].

<a id="x-28TRY-3A-2ACOLLECT-2A-20VARIABLE-29"></a>

- [variable] **\*COLLECT\*** *UNEXPECTED*

    To save memory, only the [`UNEXPECTED`][d6ad] are collected by default.
    See [Collecting Events][52e5].

<a id="x-28TRY-3A-2ARERUN-2A-20VARIABLE-29"></a>

- [variable] **\*RERUN\*** *UNEXPECTED*

    The default matches that of [`*COLLECT*`][307c]. See [Rerunning Trials][e4ac].

<a id="x-28TRY-3A-2APRINT-2A-20VARIABLE-29"></a>

- [variable] **\*PRINT\*** *LEAF*

    With the default of [`LEAF`][f58d] combined with the default [`*PRINT-PARENT*`][cc23]
    `T`, only [`TRIAL`][99d0]s with checks or [`ERROR*`][0321] in them are printed. If
    [`UNEXPECTED`][d6ad], only the interesting things are printed. See [Printing Events][b3f9].

<a id="x-28TRY-3A-2ADESCRIBE-2A-20VARIABLE-29"></a>

- [variable] **\*DESCRIBE\*** *(OR UNEXPECTED FAILURE)*

    By default, the context (e.g. [Captures][3d27], and the `CTX` argument
    of is and other checks) of [`UNEXPECTED`][d6ad] events is described. See
    [Printing Events][b3f9].

<a id="x-28TRY-3A-2ASTREAM-2A-20VARIABLE-29"></a>

- [variable] **\*STREAM\*** *(MAKE-SYNONYM-STREAM '\*DEBUG-IO\*)*

<a id="x-28TRY-3A-2APRINTER-2A-20VARIABLE-29"></a>

- [variable] **\*PRINTER\*** *TREE-PRINTER*

<a id="x-28TRY-3A-40EXPLICIT-TRY-20MGL-PAX-3ASECTION-29"></a>

### 7.2 Explicit `TRY`

Instead of invoking the test function directly, tests can also be
run by invoking the [`TRY`][b602] function.

```common-lisp
(deftest my-test ()
  (is t))

(try 'my-test)
.. MY-TEST
..   ⋅ (IS T)
.. ⋅ MY-TEST ⋅1
..
==> #<TRIAL (MY-TEST) EXPECTED-SUCCESS 0.000s ⋅1>
```

The situation is similar with a [`WITH-TEST`][8f5d], only that `TRY` wraps an
extra [`TRIAL`][99d0] around the execution of the `LAMBDA`([`0`][e400] [`1`][5c01]) to ensure that all
[`EVENT`][955d]s are signalled within a trial.

```
(try (lambda ()
       (with-test (my-test)
         (is t))))
.. (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>)
..   MY-TEST
..     ⋅ (IS T)
..   ⋅ MY-TEST ⋅1
.. ⋅ (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>) ⋅1
..
==> #<TRIAL (TRY #<FUNCTION (LAMBDA ()) {531FE50B}>) EXPECTED-SUCCESS 0.000s ⋅1>
```

Invoking tests with an explicit `TRY` is very similar to just calling
the test functions directly (see [Calling Test Functions][012f]). The differences
are that `TRY`

- can run [Testables][cdc3],

- has a function argument for each of the [`*DEBUG*`][856d], [`*COLLECT*`][307c], etc
  variables.

Those arguments default to [`*TRY-DEBUG*`][18ff], [`*TRY-COLLECT*`][0c39], etc, which
parallel and default to `*DEBUG*`, `*COLLECT*`, etc if set to
`:UNSPECIFIED`. `*TRY-DEBUG*` is `NIL`, the rest of them are `:UNSPECIFIED`.

These defaults encourage the use of an explicit `TRY` call in the
non-interactive case and calling the test functions directly in the
interactive one, but this is not enforced in any way.

<a id="x-28TRY-3ATRY-20FUNCTION-29"></a>

- [function] **TRY** *TESTABLE &KEY (DEBUG \*TRY-DEBUG\*) (COUNT \*TRY-COUNT\*) (COLLECT \*TRY-COLLECT\*) (RERUN \*TRY-RERUN\*) (PRINT \*TRY-PRINT\*) (DESCRIBE \*TRY-DESCRIBE\*) (STREAM \*TRY-STREAM\*) (PRINTER \*TRY-PRINTER\*)*

    `TRY` runs `TESTABLE` and handles the [`EVENT`][955d]s to collect, debug, print
    the results of checks and trials, and to decide what tests to skip
    and what to rerun.
    
    `DEBUG`, `COUNT`, `COLLECT`, `RERUN`, `PRINT`, and `DESCRIBE` must all be valid
    specifiers for types that are either `NIL` (the empty type) or have a
    non-empty intersection with the type [`EVENT`][955d] (e.g. `T`, [`OUTCOME`][2656],
    [`UNEXPECTED`][d6ad], [`VERDICT`][52e1]).
    
    `TRY` sets up a [`HANDLER-BIND`][fd3c] handler for `EVENT`s and runs `TESTABLE` (see
    [Testables][cdc3]). When an `EVENT` is signalled, the handler matches its
    type to the value of the `DEBUG` argument (in the sense of `(TYPEP
    EVENT DEBUG)`). If it matches, then the debugger is invoked with the
    event. In the debugger, the user has a number of restarts available
    to change (see [Event Restarts][d4ce], [Outcome Restarts][7ef5],
    [Check Restarts][2364], [Trial Restarts][5355], and [`SET-TRY-DEBUG`][f001].
    
    If the debugger is not invoked, `TRY` invokes the very first restart
    available, which is always [`RECORD-EVENT`][ce49].
    
    Recording the event is performed as follows.
    
    - Outcome counts are updated (see [Counting Events][886e]).
    
    - The event is passed to the collector (see [Collecting Events][52e5]).
    
    - The event is passed to the printer (see [Printing Events][b3f9]).
    
    - Finally, when rerunning a trial (i.e. when `TESTABLE` is a trial),
      on a [`TRIAL-START`][b664] event, the trial may be skipped (see [Rerunning Trials][e4ac]).
    
    `TRY` returns the values returned by the outermost trial (see [Tests][dc28]).

<a id="x-28TRY-3ASET-TRY-DEBUG-20FUNCTION-29"></a>

- [function] **SET-TRY-DEBUG** *DEBUG*

    Invoke the `SET-TRY-DEBUG` restart to override the `DEBUG` argument of
    the currently running [`TRY`][b602]. `DEBUG` must thus be a suitable type. When
    the `SET-TRY-DEBUG` restart is invoked interactively, `DEBUG` is read as
    a non-evaluated form from [`*QUERY-IO*`][f4bf].

<a id="x-28TRY-3A-2ATRY-DEBUG-2A-20VARIABLE-29"></a>

- [variable] **\*TRY-DEBUG\*** *NIL*

    The default value for [`TRY`][b602]'s `:DEBUG` argument. If
    `:UNSPECIFIED`, then the value of [`*DEBUG*`][856d] is used instead.

<a id="x-28TRY-3A-2ATRY-COUNT-2A-20VARIABLE-29"></a>

- [variable] **\*TRY-COUNT\*** *:UNSPECIFIED*

    The default value for [`TRY`][b602]'s `:COUNT` argument. If
    `:UNSPECIFIED`, then the value of [`*COUNT*`][3bb4] is used instead.

<a id="x-28TRY-3A-2ATRY-COLLECT-2A-20VARIABLE-29"></a>

- [variable] **\*TRY-COLLECT\*** *:UNSPECIFIED*

    The default value for [`TRY`][b602]'s `:COLLECT` argument. If
    `:UNSPECIFIED`, then the value of [`*COLLECT*`][307c] is used instead.

<a id="x-28TRY-3A-2ATRY-RERUN-2A-20VARIABLE-29"></a>

- [variable] **\*TRY-RERUN\*** *:UNSPECIFIED*

    The default value for [`TRY`][b602]'s `:RERUN` argument. If
    `:UNSPECIFIED`, then the value of [`*RERUN*`][63db] is used instead.

<a id="x-28TRY-3A-2ATRY-PRINT-2A-20VARIABLE-29"></a>

- [variable] **\*TRY-PRINT\*** *:UNSPECIFIED*

    The default value for [`TRY`][b602]'s `:PRINT` argument. If
    `:UNSPECIFIED`, then the value of [`*PRINT*`][7ee9] is used instead.

<a id="x-28TRY-3A-2ATRY-DESCRIBE-2A-20VARIABLE-29"></a>

- [variable] **\*TRY-DESCRIBE\*** *:UNSPECIFIED*

    The default value for [`TRY`][b602]'s `:DESCRIBE` argument. If
    `:UNSPECIFIED`, then the value of [`*DESCRIBE*`][aa6d] is used instead.

<a id="x-28TRY-3A-2ATRY-STREAM-2A-20VARIABLE-29"></a>

- [variable] **\*TRY-STREAM\*** *:UNSPECIFIED*

    The default value for [`TRY`][b602]'s `:STREAM` argument. If
    `:UNSPECIFIED`, then the value of [`*STREAM*`][0126] is used instead.

<a id="x-28TRY-3A-2ATRY-PRINTER-2A-20VARIABLE-29"></a>

- [variable] **\*TRY-PRINTER\*** *:UNSPECIFIED*

    The default value for [`TRY`][b602]'s `:PRINTER` argument. If
    `:UNSPECIFIED`, then the value of [`*PRINTER*`][7230] is used instead.

<a id="x-28TRY-3A-2AN-RECENT-TRIALS-2A-20VARIABLE-29"></a>

- [variable] **\*N-RECENT-TRIALS\*** *3*

    See `*RECENT-TRIALS*`.

<a id="x-28TRY-3ARECENT-TRIAL-20FUNCTION-29"></a>

- [function] **RECENT-TRIAL** *&OPTIONAL (N 0)*

    Returns the `N`th most recent trial or `NIL` if there are not enough
    trials recorded. Every [`TRIAL`][99d0] returned by [`TRY`][b602] gets pushed
    onto a list of trials, but only [`*N-RECENT-TRIALS*`][8f9f] are kept.

<a id="x-28TRY-3A-21-20VARIABLE-29"></a>

- [variable] **!** *NIL*

    The most recent trial. Equivalent to `(RECENT-TRIAL 0)`.

<a id="x-28TRY-3A-21-21-20VARIABLE-29"></a>

- [variable] **!!** *NIL*

    Equivalent to `(RECENT-TRIAL 1)`.

<a id="x-28TRY-3A-21-21-21-20VARIABLE-29"></a>

- [variable] **!!!** *NIL*

    Equivalent to `(RECENT-TRIAL 2)`.

<a id="x-28TRY-3A-40TESTABLES-20MGL-PAX-3ASECTION-29"></a>

#### 7.2.1 Testables

Valid first arguments to [`TRY`][b602] are called testables. A testable may
be:

- a [function designator][8aea]

    - the name of a global test

    - the name of a global function

    - a function object

    - a trial

- a list of testables

- a [`PACKAGE`][1d5a]

In the function designator cases, `TRY` calls the designated function.
[`TRIAL`][99d0]s, being [funcallable instance][2eef]s, designate themselves.
If the trial is not [`RUNNINGP`][5d4a], then it will be rerun (see [Rerunning Trials][e4ac]).
Don't invoke `TRY` with `RUNNINGP` trials (but see
[Implementation of Implicit `TRY`][8b9c] for discussion).

When given a list of testables, `TRY` calls each testable one by one.

Finally, a `PACKAGE` stands for the result of calling
[`LIST-PACKAGE-TESTS`][b426] on that package.

<a id="x-28TRY-3A-40IMPLICIT-TRY-IMPLEMENTATION-20MGL-PAX-3ASECTION-29"></a>

#### 7.2.2 Implementation of Implicit `TRY`

What's happening in the implementation is that a test function,
when it is called, checks whether it is running under the [`TRY`][b602]
function. If it isn't, then it invokes `TRY` with its [`TRIAL`][99d0]. `TRY`
realizes the trial cannot be rerun yet (see [Rerunning Trials][e4ac]) because it
is [`RUNNINGP`][5d4a], sets up its event handlers for debugging, collecting,
printing, and invokes the trial as if it were rerun but without
skipping anything based on the `RERUN` argument. Thus the following
are infinite recursions:

```
(with-test (recurse)
  (try recurse))

(with-test (recurse)
  (funcall recurse))
```


<a id="x-28TRY-3A-40PRINT-20MGL-PAX-3ASECTION-29"></a>

### 7.3 Printing Events

[`TRY`][b602] instantiates a printer of the type given by its `PRINTER`
argument. All [`EVENT`][955d]s recorded by `TRY` are sent to this printer. The
printer then prints events that match the type given by the `PRINT`
argument of `TRY`. Events that also match the `DESCRIBE` argument of `TRY`
are printed with context information (see [`IS`][80d6]) and backtraces (see
[`UNHANDLED-ERROR`][8f78]).

Although the printing is primarily customized with global special
variables, changing the value of those variables after the printer
object is instantiated by `TRY` has no effect. This is to ensure
consistent output with nested `TRY` calls of differing printer
setups.

<a id="x-28TRY-3ATREE-PRINTER-20CLASS-29"></a>

- [class] **TREE-PRINTER**

    `TREE-PRINTER` prints events in an indented
    tree-like structure, with each internal node corresponding to a
    [`TRIAL`][99d0]. This is the default printer (according to [`*PRINTER*`][7230] and
    [`*TRY-PRINTER*`][c864]) and currently the only one.
    
    The following example prints all [Concrete Events][279a].
    
    ```common-lisp
    (let ((*debug* nil)
          (*print* '(not trial-start))
          (*describe* nil))
      (with-test (verdict-abort*)
        (with-test (expected-verdict-success))
        (with-expected-outcome ('failure)
          (with-test (unexpected-verdict-success)))
        (handler-bind (((and verdict success) #'force-expected-failure))
          (with-test (expected-verdict-failure)))
        (handler-bind (((and verdict success) #'force-unexpected-failure))
          (with-test (unexpected-verdict-failure)))
        (with-test (verdict-skip)
          (skip-trial))
        (is t :msg "EXPECTED-RESULT-SUCCESS")
        (with-failure-expected ('failure)
          (is t :msg "UNEXPECTED-RESULT-SUCCESS")
          (is nil :msg "EXPECTED-RESULT-FAILURE"))
        (is nil :msg "UNEXPECTED-RESULT-FAILURE")
        (with-skip ()
          (is nil :msg "RESULT-SKIP"))
        (handler-bind (((and result success) #'abort-check))
          (is t :msg "RESULT-ABORT*"))
        (catch 'foo
          (with-test (nlx-test)
            (throw 'foo nil)))
        (error "UNHANDLED-ERROR")))
    .. VERDICT-ABORT*                       ; TRIAL-START
    ..   ⋅ EXPECTED-VERDICT-SUCCESS
    ..   ⊡ UNEXPECTED-VERDICT-SUCCESS
    ..   × EXPECTED-VERDICT-FAILURE
    ..   ⊠ UNEXPECTED-VERDICT-FAILURE
    ..   - VERDICT-SKIP
    ..   ⋅ EXPECTED-RESULT-SUCCESS
    ..   ⊡ UNEXPECTED-RESULT-SUCCESS
    ..   × EXPECTED-RESULT-FAILURE
    ..   ⊠ UNEXPECTED-RESULT-FAILURE
    ..   - RESULT-SKIP
    ..   ⊟ RESULT-ABORT*
    ..   NLX-TEST                           ; TRIAL-START
    ..     ⊟ non-local exit                 ; NLX
    ..   ⊟ NLX-TEST ⊟1                      ; VERDICT-ABORT*
    ..   ⊟ "UNHANDLED-ERROR" (SIMPLE-ERROR)
    .. ⊟ VERDICT-ABORT* ⊟3 ⊠1 ⊡1 -1 ×1 ⋅1
    ..
    ==> #<TRIAL (WITH-TEST (VERDICT-ABORT*)) ABORT* 0.004s ⊟3 ⊠1 ⊡1 -1 ×1 ⋅1>
    ```
    
    The `⊟3 ⊠1 ⊡1 -1 ×1 ⋅1` part is the counts for [`*CATEGORIES*`][e949] printed
    with their markers.

<a id="x-28TRY-3A-2APRINT-PARENT-2A-20VARIABLE-29"></a>

- [variable] **\*PRINT-PARENT\*** *T*

    When an [`EVENT`][955d] is signalled and its parent [`TRIAL`][99d0]'s type matches
    `*PRINT-PARENT*`, the trial is printed as if its [`TRIAL-START`][b664] matched
    the `PRINT` argument of [`TRY`][b602].
    
    ```common-lisp
    (let ((*print* 'leaf)
          (*print-parent* t))
      (with-test (t0)
        (is t)
        (is t)))
    .. T0
    ..   ⋅ (IS T)
    ..   ⋅ (IS T)
    .. ⋅ T0 ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s ⋅2>
    ```
    
    ```common-lisp
    (let ((*print* 'leaf)
          (*print-parent* nil))
      (with-test (t0)
        (is t)
        (is t)))
    .. ⋅ (IS T)
    .. ⋅ (IS T)
    ..
    ==> #<TRIAL (WITH-TEST (T0)) EXPECTED-SUCCESS 0.000s ⋅2>
    ```
    
    `*PRINT-PARENT*` `NIL` combined with printing [`VERDICT`][52e1]s results in a flat
     output:
    
    ```common-lisp
    (let ((*print* '(or leaf verdict))
          (*print-parent* nil))
      (with-test (outer)
        (with-test (inner)
          (is t :msg "inner-t"))
        (is t :msg "outer-t")))
    .. ⋅ inner-t
    .. ⋅ INNER ⋅1
    .. ⋅ outer-t
    .. ⋅ OUTER ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (OUTER)) EXPECTED-SUCCESS 0.000s ⋅2>
    ```


<a id="x-28TRY-3A-2APRINT-INDENTATION-2A-20VARIABLE-29"></a>

- [variable] **\*PRINT-INDENTATION\*** *2*

    The number of spaces each printed [`TRIAL`][99d0] increases the indentation
    of its children.

<a id="x-28TRY-3A-2APRINT-DURATION-2A-20VARIABLE-29"></a>

- [variable] **\*PRINT-DURATION\*** *NIL*

    If true, the number of seconds spent during execution is printed.
    
    ```common-lisp
    (let ((*print-duration* t)
          (*debug* nil)
          (*describe* nil))
      (with-test (timed)
        (is (progn (sleep 0.3) t))
        (is (progn (sleep 0.2) t))
        (error "xxx")))
    ..        TIMED
    ..  0.300   ⋅ (IS (PROGN (SLEEP 0.3) T))
    ..  0.200   ⋅ (IS (PROGN (SLEEP 0.2) T))
    ..          ⊟ ""xxx (SIMPLE-ERROR)
    ..  0.504 ⊟ TIMED ⊟1 ⋅2
    ..
    ==> #<TRIAL (WITH-TEST (TIMED)) ABORT* 0.504s ⊟1 ⋅2>
    ```
    
    Timing is available for all [`OUTCOME`][2656]s (i.e. for [Checks][bb56] and [`TRIAL`][99d0]s).
    Checks generally measure the time spent during evaluation the form
    they are wrapping. Trials measure the time between [`TRIAL-START`][b664] and
    the [`VERDICT`][52e1].
    
    Timing information is not available for `TRIAL-START` and [`ERROR*`][0321]
    events.

<a id="x-28TRY-3A-2APRINT-COMPACTLY-2A-20VARIABLE-29"></a>

- [variable] **\*PRINT-COMPACTLY\*** *NIL*

    [`EVENT`][955d]s whose type matches `*PRINT-COMPACTLY*` are printed less
    verbosely. [`LEAF`][f58d] events are printed only with their marker, and
    [`VERDICT`][52e1]s of trials without printed child trials are printed with `=>
    <MARKER>` (see [`*CATEGORIES*`][e949]).
    
    ```common-lisp
    (let ((*print-compactly* t)
          (*debug* nil)
          (*describe* nil))
      (with-test (outer)
        (loop repeat 10 do (is t))
        (with-test (inner)
          (is t)
          (is nil)
          (error "xxx"))
        (loop repeat 10 do (is t))))
    .. OUTER ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
    ..   INNER ⋅⊠⊟ => ⊟
    ..   ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
    .. ⊠ OUTER ⊟1 ⊠1 ⋅21
    ..
    ==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊟1 ⊠1 ⋅21>
    ```
    
    `*PRINT-COMPACTLY*` has no effect on events being [`DESCRIBE`][6651]d.

<a id="x-28TRY-3A-2ADEFER-DESCRIBE-2A-20VARIABLE-29"></a>

- [variable] **\*DEFER-DESCRIBE\*** *NIL*

    When an [`EVENT`][955d] is to be [`*DESCRIBE*`][aa6d]d and its type matches
    `*DEFER-DESCRIBE*`, then instead of printing the often longish context
    information in the tree of events, it is deferred until after [`TRY`][b602]
    has finished. The following example only prints [`LEAF`][f58d] events (due to
    [`*PRINT*`][7ee9] and [`*PRINT-PARENT*`][cc23]) and in compact form (see
    [`*PRINT-COMPACTLY*`][3cf4]), deferring description of events matching
    `*DESCRIBE*` until the end.
    
    ```common-lisp
    (let ((*print* 'leaf)
          (*print-parent* nil)
          (*print-compactly* t)
          (*defer-describe* t)
          (*debug* nil))
      (with-test (outer)
        (loop repeat 10 do (is t))
        (with-test (inner)
          (is (= (1+ 5) 7)))))
    .. ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⊠
    ..
    .. ;; UNEXPECTED-RESULT-FAILURE (⊠) in OUTER INNER:
    .. (IS (= #1=(1+ 5) 7))
    .. where
    ..   #1# = 6
    ..
    ==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅10>
    ```


<a id="x-28TRY-3A-40COUNT-20MGL-PAX-3ASECTION-29"></a>

### 7.4 Counting Events

[`TRIAL`][99d0]s have a counter for each category in [`*CATEGORIES*`][e949]. When an
[`EVENT`][955d] is recorded by [`TRY`][b602] and its type matches [`*COUNT*`][3bb4], the counters
of all categories matching the event type are incremented in the
[`CURRENT-TRIAL`][e186]. When a trial finishes and a [`VERDICT`][52e1] is recorded, the
trial's event counters are added to that of its parent's (if any).
The counts are printed with `VERDICT`s (see [Printing Events][b3f9]).

If both `*COUNT*` and `*CATEGORIES*` are unchanged from the their
default values, then only [`LEAF`][f58d] events are counted, and we get
separate counters for [`ABORT*`][8ec3], [`UNEXPECTED-FAILURE`][b5cb],
[`UNEXPECTED-SUCCESS`][55cd], [`SKIP`][69a2], [`EXPECTED-FAILURE`][8620], and [`EXPECTED-SUCCESS`][c96a].

```common-lisp
(let ((*debug* nil))
  (with-test (outer)
    (with-test (inner)
      (is t))
    (is t)
    (is nil)))
.. OUTER
..   INNER
..     ⋅ (IS T)
..   ⋅ INNER ⋅1
..   ⋅ (IS T)
..   ⊠ (IS NIL)
.. ⊠ OUTER ⊠1 ⋅2
..
==> #<TRIAL (WITH-TEST (OUTER)) UNEXPECTED-FAILURE 0.000s ⊠1 ⋅2>
```

As the above example shows, [`EXPECTED-VERDICT-SUCCESS`][06c2] and
[`EXPECTED-RESULT-SUCCESS`][609c7] are both marked with `"⋅"`, but only
`EXPECTED-RESULT-SUCCESS` is counted due to `*COUNT*` being `LEAF`.

<a id="x-28TRY-3A-40COLLECT-20MGL-PAX-3ASECTION-29"></a>

### 7.5 Collecting Events

When an [`EVENT`][955d] is recorded and the type of the `EVENT` matches the
`COLLECT` type argument of [`TRY`][b602], then a corresponding object is pushed
onto [`CHILDREN`][de7d] of the [`CURRENT-TRIAL`][e186] for subsequent [Rerunning Trials][e4ac] or [Reprocessing Trials][2337].

In particular, if the matching event is a [`LEAF`][f58d], then the event
itself is collected. If the matching event is a [`TRIAL-EVENT`][b36a], then
its [`TRIAL`][0f05] is collected. Furthermore, trials
which collected anything are always collected by their parent.

By default, both implicit and explicit calls to `TRY` collect the
[`UNEXPECTED`][d6ad] (see [`*COLLECT*`][307c] and [`*TRY-COLLECT*`][0c39]), and consequently all
the enclosing trials.

<a id="x-28TRY-3ACHILDREN-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29"></a>

- [reader] **CHILDREN** *[TRIAL][99d0] (:CHILDREN = NIL)*

    A list of immediate child [`VERDICT`][52e1]s, [`RESULT`][231f]s, and
    [`ERROR*`][0321]s collected in reverse chronological order (see [Collecting Events][52e5]).
    The `VERDICT` of this [`TRIAL`][99d0] is not among `CHILDREN`, but the `VERDICT`s
    of child trials' are.

<a id="x-28TRY-3A-40RERUN-20MGL-PAX-3ASECTION-29"></a>

### 7.6 Rerunning Trials

When a [`TRIAL`][99d0] is [`FUNCALL`][03c7]ed or passed to [`TRY`][b602], the *test that
created the trial* is invoked, and it may be run again in its
entirety or in part. As the test runs, it may invoke other tests.
Any test (including the top-level one) is skipped if it does not
correspond to a [collected][52e5] trial or its [`TRIAL-START`][b664] event
and [`VERDICT`][52e1] do not match the `RERUN` argument of `TRY`. When that
happens, the corresponding function call immediately returns the
`TRIAL` object.

- A new trial is skipped (as if with [`SKIP-TRIAL`][f45a]) if `RERUN` is not `T`
  and

    - there is no trial representing the same function call among
      the collected but not yet rerun trials in the trial being
      rerun, or

    - the first such trial does not match the `RERUN` type argument of
      `TRY` in that neither its `TRIAL-START`, `VERDICT` events match the
      type `RERUN`, nor do any of its collected [`RESULT`][231f]s and trials.

- The *test that created the trial* is determined as follows.

    - If the trial was created by calling a [`DEFTEST`][e7ca] function, then
      the test currently associated with that symbol naming the
      function is called with the arguments of the original function
      call. If the symbol is no longer [`FBOUNDP`][95bb] (because it was
      [`FMAKUNBOUND`][609c]) or it no longer names a `DEFTEST` (it was redefined
      with [`DEFUN`][f472]), then an error is signalled.

    - If the trial was created by entering a [`WITH-TEST`][8f5d] form, then
      its body is executed again in the original lexical but the
      current dynamic environment. Implementationally speaking,
      `WITH-TEST` defines a local function of no arguments (likely a
      closure) that wraps its body, stores the closure in the trial
      object and calls it on a rerun in a `WITH-TEST` of the same
      `TRIAL-VAR` and same `NAME`.

    - If the trial was created by `TRY` itself to ensure that all
      events are signalled in a trial (see [Explicit `TRY`][1720]), then
      on a rerun the same `TESTABLE` is run again.

    All three possibilities involve entering `DEFTEST` or `WITH-TEST`,
    or invoking `TRY`: the same cases that we have when calling tests
    functions (see [Calling Test Functions][012f]). Thus, even if a trial is rerun
    with `FUNCALL`, execution is guaranteed to happen under `TRY`.


<a id="x-28TRY-3A-40REPLAY-20MGL-PAX-3ASECTION-29"></a>

### 7.7 Reprocessing Trials

<a id="x-28TRY-3AREPLAY-EVENTS-20FUNCTION-29"></a>

- [function] **REPLAY-EVENTS** *TRIAL &KEY (COLLECT \*TRY-COLLECT\*) (PRINT \*TRY-PRINT\*) (DESCRIBE \*TRY-DESCRIBE\*) (STREAM \*TRY-STREAM\*) (PRINTER \*TRY-PRINTER\*)*

    `REPLAY-EVENTS` reprocesses the events collected (see [Collecting Events][52e5])
    in `TRIAL`. It takes the same arguments as [`TRY`][b602] except
    `DEBUG`, `COUNT` and `RERUN`. This is because
    `REPLAY-EVENTS` does not run any tests. It simply signals the events
    collected in `TRIAL` again to allow further processing. The values of
    [`*CATEGORIES*`][e949] and [`*COUNT*`][3bb4] that were in effect for `TRIAL` are used, and
    their current values are ignored to be able to keep consistent
    counts (see [Counting Events][886e]).
    
    Suppose we have run a large test using the default `:PRINT 'LEAF`
    `:COLLECT 'UNEXPECTED` arguments for `TRY`, and now we have too much
    output to look at. Instead of searching for the interesting bits in
    the output, we can replay the events and print only the [`UNEXPECTED`][d6ad]
    events:
    
    ```
    (replay-events ! :print 'unexpected)
    ```
    
    Or we could tell the printer to just print markers for `*CATEGORIES*`
    and `:DESCRIBE` at the end:
    
    ```
    (let ((*print-parent* nil)
          (*print-compactly* t)
          (*defer-describe* t)
          (*categories* (ascii-std-categories)))
      (replay-events !))
    .. ................F................!.....
    .. 
    .. ;; UNEXPECTED-FAILURE (F) in SOME-TEST INNER-TEST:
    .. (IS (= 5 6))
    .. debug info
    .. 
    .. ;; UNHANDLED-ERROR (!) in SOME-TEST:
    .. "my-msg" (MY-ERR)
    ```


<a id="x-28TRY-3A-40IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29"></a>

## 8 Implementation Notes

Try is supported on ABCL, AllegroCL, CLISP, CCL, CMUCL, ECL and
SBCL.

- Pretty printing is non-existent on CLISP and broken on ABCL. The
  output may look garbled on them.

- Gray streams are broken on ABCL so the output may look even worse
  [https://abcl.org/trac/ticket/373](https://abcl.org/trac/ticket/373).

- ABCL, CMUCL, and ECL have a bug related to losing
  [`EQL`][db03]ness of source literals
  [https://gitlab.com/embeddable-common-lisp/ecl/-/issues/665](https://gitlab.com/embeddable-common-lisp/ecl/-/issues/665).
  The result is somewhat cosmetic, it may cause multiple captures
  being made for the same thing.


<a id="x-28TRY-3A-40GLOSSARY-20MGL-PAX-3ASECTION-29"></a>

## 9 Glossary

<a id="x-28TRY-3A-40FUNCALLABLE-INSTANCE-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **funcallable instance**

    This is a term from the MOP. A funcallable instance is an instance
    of a class that's a subclass of `MOP:FUNCALLABLE-STANDARD-CLASS`. It
    is like a normal instance, but it can also be [`FUNCALL`][03c7]ed.

<a id="x-28TRY-3A-40CANCELLED-NLX-20MGL-PAX-3AGLOSSARY-TERM-29"></a>

- [glossary-term] **cancelled non-local exit**

    This is a term from the Common Lisp ANSI standard. If during the
    unwinding of the stack initiated by a [non-local exit][b815] another
    nlx is initiated in, and exits from an [`UNWIND-PROTECT`][c93f] cleanup form,
    then this second nlx is said to have cancelled the first, and the
    first nlx will not continue.
    
    ```common-lisp
    (catch 'foo
      (catch 'bar
        (unwind-protect
             (throw 'foo 'foo)
          (throw 'bar 'bar))))
    => BAR
    ```


  [0126]: #x-28TRY-3A-2ASTREAM-2A-20VARIABLE-29 "TRY:*STREAM* VARIABLE"
  [012f]: #x-28TRY-3A-40IMPLICIT-TRY-20MGL-PAX-3ASECTION-29 "Calling Test Functions"
  [02a3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_abortc.htm "CONTINUE (MGL-PAX:CLHS FUNCTION)"
  [0321]: #x-28TRY-3AERROR-2A-20CONDITION-29 "TRY:ERROR* CONDITION"
  [03c7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm "FUNCALL (MGL-PAX:CLHS FUNCTION)"
  [03ec]: #x-28TRY-3A-40CATEGORIES-20MGL-PAX-3ASECTION-29 "Categories"
  [062e]: #x-28TRY-3AUNEXPECTED-VERDICT-SUCCESS-20CONDITION-29 "TRY:UNEXPECTED-VERDICT-SUCCESS CONDITION"
  [06c2]: #x-28TRY-3AEXPECTED-VERDICT-SUCCESS-20CONDITION-29 "TRY:EXPECTED-VERDICT-SUCCESS CONDITION"
  [0743]: #x-28TRY-3A-40WRITING-AUTOMATIC-CAPTURE-RULES-20MGL-PAX-3ASECTION-29 "Writing Automatic Capture Rules"
  [0992]: #x-28TRY-3ADISMISSAL-20CONDITION-29 "TRY:DISMISSAL CONDITION"
  [0c39]: #x-28TRY-3A-2ATRY-COLLECT-2A-20VARIABLE-29 "TRY:*TRY-COLLECT* VARIABLE"
  [0d57]: http://www.lispworks.com/documentation/HyperSpec/Body/t_short_.htm "DOUBLE-FLOAT (MGL-PAX:CLHS TYPE)"
  [0e59]: http://www.lispworks.com/documentation/HyperSpec/Body/f_gensym.htm "GENSYM (MGL-PAX:CLHS FUNCTION)"
  [0f05]: #x-28TRY-3ATRIAL-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-EVENT-29-29 "TRY:TRIAL (MGL-PAX:READER TRY:TRIAL-EVENT)"
  [1013]: http://www.lispworks.com/documentation/HyperSpec/Body/f_not.htm "NOT (MGL-PAX:CLHS FUNCTION)"
  [12ce]: #x-28TRY-3AINVOKES-DEBUGGER-20MGL-PAX-3AMACRO-29 "TRY:INVOKES-DEBUGGER MGL-PAX:MACRO"
  [1383]: http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm "MACROLET (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [162a]: #x-28TRY-3AMATCH-VALUES-20MGL-PAX-3AMACRO-29 "TRY:MATCH-VALUES MGL-PAX:MACRO"
  [1720]: #x-28TRY-3A-40EXPLICIT-TRY-20MGL-PAX-3ASECTION-29 "Explicit `TRY`"
  [1867]: http://www.lispworks.com/documentation/HyperSpec/Body/r_contin.htm "CONTINUE (MGL-PAX:CLHS RESTART)"
  [18ff]: #x-28TRY-3A-2ATRY-DEBUG-2A-20VARIABLE-29 "TRY:*TRY-DEBUG* VARIABLE"
  [19f3]: #x-28TRY-3ACAPTURE-20MGL-PAX-3AMACRO-29 "TRY:CAPTURE MGL-PAX:MACRO"
  [1cdc]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debugg.htm "*DEBUGGER-HOOK* (MGL-PAX:CLHS VARIABLE)"
  [1d5a]: http://www.lispworks.com/documentation/HyperSpec/Body/t_pkg.htm "PACKAGE (MGL-PAX:CLHS CLASS)"
  [1d97]: #x-28TRY-3AWITH-EXPECTED-OUTCOME-20MGL-PAX-3AMACRO-29 "TRY:WITH-EXPECTED-OUTCOME MGL-PAX:MACRO"
  [1eb3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_1pl_1_.htm "1+ (MGL-PAX:CLHS FUNCTION)"
  [2038]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stu_ob.htm "STRUCTURE-OBJECT (MGL-PAX:CLHS CLASS)"
  [20d8]: #x-28TRY-3A-40EXPLICIT-CAPTURES-20MGL-PAX-3ASECTION-29 "Explicit Captures"
  [21d9]: #x-28TRY-3APASS-20TYPE-29 "TRY:PASS TYPE"
  [231f]: #x-28TRY-3ARESULT-20CONDITION-29 "TRY:RESULT CONDITION"
  [2337]: #x-28TRY-3A-40REPLAY-20MGL-PAX-3ASECTION-29 "Reprocessing Trials"
  [2364]: #x-28TRY-3A-40CHECK-RESTARTS-20MGL-PAX-3ASECTION-29 "Check Restarts"
  [25f5]: http://www.lispworks.com/documentation/HyperSpec/Body/f_null.htm "NULL (MGL-PAX:CLHS FUNCTION)"
  [2656]: #x-28TRY-3AOUTCOME-20CONDITION-29 "TRY:OUTCOME CONDITION"
  [269a]: #x-28TRY-3ASUCCESS-20CONDITION-29 "TRY:SUCCESS CONDITION"
  [279a]: #x-28TRY-3A-40CONCRETE-EVENTS-20MGL-PAX-3ASECTION-29 "Concrete Events"
  [2eef]: #x-28TRY-3A-40FUNCALLABLE-INSTANCE-20MGL-PAX-3AGLOSSARY-TERM-29 "funcallable instance"
  [307c]: #x-28TRY-3A-2ACOLLECT-2A-20VARIABLE-29 "TRY:*COLLECT* VARIABLE"
  [30c9]: #x-28TRY-3AEXPECTED-VERDICT-FAILURE-20CONDITION-29 "TRY:EXPECTED-VERDICT-FAILURE CONDITION"
  [31a6]: http://www.lispworks.com/documentation/HyperSpec/Body/t_short_.htm "SINGLE-FLOAT (MGL-PAX:CLHS TYPE)"
  [3233]: #x-28TRY-3A-40FORMAT-SPECIFIER-FORMS-20MGL-PAX-3ASECTION-29 "Format Specifier Forms"
  [351f]: #x-28TRY-3ACAPTURE-VALUES-20MGL-PAX-3AMACRO-29 "TRY:CAPTURE-VALUES MGL-PAX:MACRO"
  [35ba]: http://www.lispworks.com/documentation/HyperSpec/Body/f_error.htm "ERROR (MGL-PAX:CLHS FUNCTION)"
  [37c1]: #x-28TRY-3A-40MISC-CHECKS-20MGL-PAX-3ASECTION-29 "Miscellaneous Checks"
  [39df]: http://www.lispworks.com/documentation/HyperSpec/Body/m_w_std_.htm "WITH-STANDARD-IO-SYNTAX (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [3ace]: #x-28TRY-3ABACKTRACE-OF-20-28MGL-PAX-3AREADER-20TRY-3AUNHANDLED-ERROR-29-29 "TRY:BACKTRACE-OF (MGL-PAX:READER TRY:UNHANDLED-ERROR)"
  [3bb4]: #x-28TRY-3A-2ACOUNT-2A-20VARIABLE-29 "TRY:*COUNT* VARIABLE"
  [3cf4]: #x-28TRY-3A-2APRINT-COMPACTLY-2A-20VARIABLE-29 "TRY:*PRINT-COMPACTLY* VARIABLE"
  [3d27]: #x-28TRY-3A-40CAPTURES-20MGL-PAX-3ASECTION-29 "Captures"
  [3e0c]: #x-28TRY-3A-40MIDDLE-LAYER-OF-EVENTS-20MGL-PAX-3ASECTION-29 "Middle Layer of Events"
  [3eef]: #x-28TRY-3A-40IMPLEMENTATION-NOTES-20MGL-PAX-3ASECTION-29 "Implementation Notes"
  [3eef7]: http://www.lispworks.com/documentation/HyperSpec/Body/s_ret_fr.htm "RETURN-FROM (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [4444]: http://www.lispworks.com/documentation/HyperSpec/Body/m_mult_1.htm "MULTIPLE-VALUE-LIST (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [4805]: #x-28TRY-3AVERDICT-ABORT-2A-20CONDITION-29 "TRY:VERDICT-ABORT* CONDITION"
  [4bec]: #x-28TRY-3AVERDICT-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29 "TRY:VERDICT (MGL-PAX:READER TRY:TRIAL)"
  [4c86]: #x-28TRY-3A-40EMACS-20MGL-PAX-3ASECTION-29 "Emacs Integration"
  [4e46]: http://www.lispworks.com/documentation/HyperSpec/Body/f_find_.htm "FIND (MGL-PAX:CLHS FUNCTION)"
  [4f9f]: #x-28TRY-3AABORT-TRIAL-20FUNCTION-29 "TRY:ABORT-TRIAL FUNCTION"
  [4fc4]: #x-28TRY-3A-40EMACS-SETUP-20MGL-PAX-3ASECTION-29 "Emacs Setup"
  [5065]: #x-28TRY-3ATEST-BOUND-P-20FUNCTION-29 "TRY:TEST-BOUND-P FUNCTION"
  [514a]: #x-28TRY-3A-40TRIAL-EVENTS-20MGL-PAX-3ASECTION-29 "Trial Events"
  [5237]: #x-28TRY-3A-40EVENT-GLUE-20MGL-PAX-3ASECTION-29 "Event Glue"
  [5289]: #x-28TRY-3AWARN-ON-TESTS-NOT-RUN-20MGL-PAX-3AMACRO-29 "TRY:WARN-ON-TESTS-NOT-RUN MGL-PAX:MACRO"
  [52e1]: #x-28TRY-3AVERDICT-20CONDITION-29 "TRY:VERDICT CONDITION"
  [52e5]: #x-28TRY-3A-40COLLECT-20MGL-PAX-3ASECTION-29 "Collecting Events"
  [5333]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "> (MGL-PAX:CLHS FUNCTION)"
  [5355]: #x-28TRY-3A-40TRIAL-RESTARTS-20MGL-PAX-3ASECTION-29 "Trial Restarts"
  [55cd]: #x-28TRY-3AUNEXPECTED-SUCCESS-20TYPE-29 "TRY:UNEXPECTED-SUCCESS TYPE"
  [56ae]: #x-28TRY-3A-40AUTOMATIC-CAPTURES-20MGL-PAX-3ASECTION-29 "Automatic Captures"
  [5786]: #x-28TRY-3AVERDICT-SKIP-20CONDITION-29 "TRY:VERDICT-SKIP CONDITION"
  [5a82]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm "EQ (MGL-PAX:CLHS FUNCTION)"
  [5c01]: http://www.lispworks.com/documentation/HyperSpec/Body/m_lambda.htm "LAMBDA (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [5d4a]: #x-28TRY-3ARUNNINGP-20FUNCTION-29 "TRY:RUNNINGP FUNCTION"
  [5e1a]: #x-28TRY-3A-40TRIAL-VERDICTS-20MGL-PAX-3ASECTION-29 "Trial Verdicts"
  [609c]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fmakun.htm "FMAKUNBOUND (MGL-PAX:CLHS FUNCTION)"
  [609c7]: #x-28TRY-3AEXPECTED-RESULT-SUCCESS-20CONDITION-29 "TRY:EXPECTED-RESULT-SUCCESS CONDITION"
  [628a]: #x-28TRY-3A-40LINKS-20MGL-PAX-3ASECTION-29 "Links and Systems"
  [6384]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRIN1 (MGL-PAX:CLHS FUNCTION)"
  [63db]: #x-28TRY-3A-2ARERUN-2A-20VARIABLE-29 "TRY:*RERUN* VARIABLE"
  [6651]: http://www.lispworks.com/documentation/HyperSpec/Body/f_descri.htm "DESCRIBE (MGL-PAX:CLHS FUNCTION)"
  [676d]: http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm "PRINC (MGL-PAX:CLHS FUNCTION)"
  [6910]: #x-28TRY-3AWITH-TESTS-RUN-20MGL-PAX-3AMACRO-29 "TRY:WITH-TESTS-RUN MGL-PAX:MACRO"
  [69a2]: #x-28TRY-3ASKIP-20CONDITION-29 "TRY:SKIP CONDITION"
  [6b0e]: #x-28TRY-3ASUB-20CLASS-29 "TRY:SUB CLASS"
  [6d4e]: #x-28TRY-3ASIGNALS-20MGL-PAX-3AMACRO-29 "TRY:SIGNALS MGL-PAX:MACRO"
  [7230]: #x-28TRY-3A-2APRINTER-2A-20VARIABLE-29 "TRY:*PRINTER* VARIABLE"
  [790c]: #x-28TRY-3A-25-20MACROLET-29 "TRY:% MACROLET"
  [7955]: #x-28TRY-3AFLOAT--7E-3D-20FUNCTION-29 "TRY:FLOAT-~= FUNCTION"
  [7ab6]: #x-28TRY-3A-40CANCELLED-NLX-20MGL-PAX-3AGLOSSARY-TERM-29 "cancelled non-local exit"
  [7af9]: #x-28TRY-3ASIGNALS-NOT-20MGL-PAX-3AMACRO-29 "TRY:SIGNALS-NOT MGL-PAX:MACRO"
  [7c3f]: #x-28TRY-3ARESULT-SKIP-20CONDITION-29 "TRY:RESULT-SKIP CONDITION"
  [7ee9]: #x-28TRY-3A-2APRINT-2A-20VARIABLE-29 "TRY:*PRINT* VARIABLE"
  [7ef5]: #x-28TRY-3A-40OUTCOME-RESTARTS-20MGL-PAX-3ASECTION-29 "Outcome Restarts"
  [7f8e]: #x-28TRY-3A-40ERRORS-20MGL-PAX-3ASECTION-29 "Errors"
  [80d6]: #x-28TRY-3AIS-20MGL-PAX-3AMACRO-29 "TRY:IS MGL-PAX:MACRO"
  [826a]: #x-28TRY-3AABORT-CHECK-20FUNCTION-29 "TRY:ABORT-CHECK FUNCTION"
  [83e1]: http://www.lispworks.com/documentation/HyperSpec/Body/e_cnd.htm "CONDITION (MGL-PAX:CLHS CONDITION)"
  [856d]: #x-28TRY-3A-2ADEBUG-2A-20VARIABLE-29 "TRY:*DEBUG* VARIABLE"
  [8620]: #x-28TRY-3AEXPECTED-FAILURE-20TYPE-29 "TRY:EXPECTED-FAILURE TYPE"
  [886e]: #x-28TRY-3A-40COUNT-20MGL-PAX-3ASECTION-29 "Counting Events"
  [8aea]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#function_designator '"function designator" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [8b69]: #x-28TRY-3AREPLAY-EVENTS-20FUNCTION-29 "TRY:REPLAY-EVENTS FUNCTION"
  [8b9c]: #x-28TRY-3A-40IMPLICIT-TRY-IMPLEMENTATION-20MGL-PAX-3ASECTION-29 "Implementation of Implicit `TRY`"
  [8cf6]: #x-28TRY-3ARETRY-CHECK-20FUNCTION-29 "TRY:RETRY-CHECK FUNCTION"
  [8ec3]: #x-28TRY-3AABORT-2A-20CONDITION-29 "TRY:ABORT* CONDITION"
  [8f5d]: #x-28TRY-3AWITH-TEST-20MGL-PAX-3AMACRO-29 "TRY:WITH-TEST MGL-PAX:MACRO"
  [8f78]: #x-28TRY-3AUNHANDLED-ERROR-20CONDITION-29 "TRY:UNHANDLED-ERROR CONDITION"
  [8f7a]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_lev.htm "*PRINT-LENGTH* (MGL-PAX:CLHS VARIABLE)"
  [8f9f]: #x-28TRY-3A-2AN-RECENT-TRIALS-2A-20VARIABLE-29 "TRY:*N-RECENT-TRIALS* VARIABLE"
  [906a]: #x-28TRY-3A-40CHECK-UTILITIES-20MGL-PAX-3ASECTION-29 "Check Utilities"
  [92af]: #x-28TRY-3A-21-20VARIABLE-29 "TRY:! VARIABLE"
  [955d]: #x-28TRY-3AEVENT-20CONDITION-29 "TRY:EVENT CONDITION"
  [95bb]: http://www.lispworks.com/documentation/HyperSpec/Body/f_fbound.htm "FBOUNDP (MGL-PAX:CLHS FUNCTION)"
  [97ee]: http://www.lispworks.com/documentation/HyperSpec/Body/m_assert.htm "ASSERT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [99d0]: #x-28TRY-3ATRIAL-20CLASS-29 "TRY:TRIAL CLASS"
  [9c9c]: http://www.lispworks.com/documentation/HyperSpec/Body/s_eval_w.htm "EVAL-WHEN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [9fa9]: #x-28TRY-3A-40COMPARING-FLOATS-20MGL-PAX-3ASECTION-29 "Comparing Floats"
  [a07f]: #x-28TRY-3A-2ABEST-MATCHING-CONDITION-2A-20VARIABLE-29 "TRY:*BEST-MATCHING-CONDITION* VARIABLE"
  [aa6d]: #x-28TRY-3A-2ADESCRIBE-2A-20VARIABLE-29 "TRY:*DESCRIBE* VARIABLE"
  [aaaa]: #x-28TRY-3AINVOKES-DEBUGGER-NOT-20MGL-PAX-3AMACRO-29 "TRY:INVOKES-DEBUGGER-NOT MGL-PAX:MACRO"
  [aaf2]: #x-28TRY-3A-40EVENTS-20MGL-PAX-3ASECTION-29 "Events"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [afb9]: #x-28TRY-3A-40PRINTING-EVENTS-20MGL-PAX-3ASECTION-29 "Printing Events"
  [b115]: #x-28TRY-3ANLX-20CONDITION-29 "TRY:NLX CONDITION"
  [b194]: #x-28TRY-3AEXPECTED-20CONDITION-29 "TRY:EXPECTED CONDITION"
  [b33f]: #x-28TRY-3AN-RETRIES-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29 "TRY:N-RETRIES (MGL-PAX:READER TRY:TRIAL)"
  [b36a]: #x-28TRY-3ATRIAL-EVENT-20CONDITION-29 "TRY:TRIAL-EVENT CONDITION"
  [b3f9]: #x-28TRY-3A-40PRINT-20MGL-PAX-3ASECTION-29 "Printing Events"
  [b426]: #x-28TRY-3ALIST-PACKAGE-TESTS-20FUNCTION-29 "TRY:LIST-PACKAGE-TESTS FUNCTION"
  [b5cb]: #x-28TRY-3AUNEXPECTED-FAILURE-20TYPE-29 "TRY:UNEXPECTED-FAILURE TYPE"
  [b602]: #x-28TRY-3ATRY-20FUNCTION-29 "TRY:TRY FUNCTION"
  [b664]: #x-28TRY-3ATRIAL-START-20CONDITION-29 "TRY:TRIAL-START CONDITION"
  [b71e]: #x-28TRY-3AWITH-SKIP-20MGL-PAX-3AMACRO-29 "TRY:WITH-SKIP MGL-PAX:MACRO"
  [b72c]: #x-28TRY-3AUNEXPECTED-RESULT-SUCCESS-20CONDITION-29 "TRY:UNEXPECTED-RESULT-SUCCESS CONDITION"
  [b815]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_n.htm#non-local_exit '"non-local exit" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)'
  [b949]: #x-28TRY-3A-40TUTORIAL-20MGL-PAX-3ASECTION-29 "Tutorial"
  [b94a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mismat.htm "MISMATCH (MGL-PAX:CLHS FUNCTION)"
  [bb56]: #x-28TRY-3A-40CHECKS-20MGL-PAX-3ASECTION-29 "Checks"
  [c1f6]: #x-28TRY-3A-25-25-20MACROLET-29 "TRY:%% MACROLET"
  [c3a0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "< (MGL-PAX:CLHS FUNCTION)"
  [c759]: #x-28TRY-3A-40GLOSSARY-20MGL-PAX-3ASECTION-29 "Glossary"
  [c864]: #x-28TRY-3A-2ATRY-PRINTER-2A-20VARIABLE-29 "TRY:*TRY-PRINTER* VARIABLE"
  [c8cb]: http://www.lispworks.com/documentation/HyperSpec/Body/v_pr_cir.htm "*PRINT-CIRCLE* (MGL-PAX:CLHS VARIABLE)"
  [c93f]: http://www.lispworks.com/documentation/HyperSpec/Body/s_unwind.htm "UNWIND-PROTECT (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [c96a]: #x-28TRY-3AEXPECTED-SUCCESS-20TYPE-29 "TRY:EXPECTED-SUCCESS TYPE"
  [cc23]: #x-28TRY-3A-2APRINT-PARENT-2A-20VARIABLE-29 "TRY:*PRINT-PARENT* VARIABLE"
  [cdba]: http://www.lispworks.com/documentation/HyperSpec/Body/f_uninte.htm "UNINTERN (MGL-PAX:CLHS FUNCTION)"
  [cdc3]: #x-28TRY-3A-40TESTABLES-20MGL-PAX-3ASECTION-29 "Testables"
  [ce49]: #x-28TRY-3ARECORD-EVENT-20FUNCTION-29 "TRY:RECORD-EVENT FUNCTION"
  [cf88]: #x-28TRY-3A-2ACONDITION-MATCHED-P-2A-20VARIABLE-29 "TRY:*CONDITION-MATCHED-P* VARIABLE"
  [cfd3]: #x-28TRY-3A-2ARUN-DEFTEST-WHEN-2A-20VARIABLE-29 "TRY:*RUN-DEFTEST-WHEN* VARIABLE"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d2d8]: http://www.lispworks.com/documentation/HyperSpec/Body/s_block.htm "BLOCK (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [d4ce]: #x-28TRY-3A-40EVENT-RESTARTS-20MGL-PAX-3ASECTION-29 "Event Restarts"
  [d5a2]: http://www.lispworks.com/documentation/HyperSpec/Body/f_car_c.htm "CAR (MGL-PAX:CLHS FUNCTION)"
  [d5ea]: #x-28TRY-3AFAIL-20TYPE-29 "TRY:FAIL TYPE"
  [d619]: #x-28TRY-3AEXPECTED-RESULT-FAILURE-20CONDITION-29 "TRY:EXPECTED-RESULT-FAILURE CONDITION"
  [d6ad]: #x-28TRY-3AUNEXPECTED-20CONDITION-29 "TRY:UNEXPECTED CONDITION"
  [daeb]: #x-28TRY-3AUNEXPECTED-RESULT-FAILURE-20CONDITION-29 "TRY:UNEXPECTED-RESULT-FAILURE CONDITION"
  [db03]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm "EQL (MGL-PAX:CLHS FUNCTION)"
  [dbd4]: http://www.lispworks.com/documentation/HyperSpec/Body/f_vals_l.htm "VALUES-LIST (MGL-PAX:CLHS FUNCTION)"
  [dc28]: #x-28TRY-3A-40TESTS-20MGL-PAX-3ASECTION-29 "Tests"
  [de7d]: #x-28TRY-3ACHILDREN-20-28MGL-PAX-3AREADER-20TRY-3ATRIAL-29-29 "TRY:CHILDREN (MGL-PAX:READER TRY:TRIAL)"
  [e186]: #x-28TRY-3ACURRENT-TRIAL-20FUNCTION-29 "TRY:CURRENT-TRIAL FUNCTION"
  [e2e0]: #x-28TRY-3A-40IS-20MGL-PAX-3ASECTION-29 "The `IS` Macro"
  [e400]: http://www.lispworks.com/documentation/HyperSpec/Body/s_lambda.htm '"s_lambda" (MGL-PAX:CLHS MGL-PAX:SECTION)'
  [e4ac]: #x-28TRY-3A-40RERUN-20MGL-PAX-3ASECTION-29 "Rerunning Trials"
  [e514]: #x-28TRY-3A-40OUTCOMES-20MGL-PAX-3ASECTION-29 "Outcomes"
  [e52f]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm "= (MGL-PAX:CLHS FUNCTION)"
  [e6be]: #x-28TRY-3A-40TRIALS-20MGL-PAX-3ASECTION-29 "Trials"
  [e760]: http://www.lispworks.com/documentation/HyperSpec/Body/s_throw.htm "THROW (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [e7ca]: #x-28TRY-3ADEFTEST-20MGL-PAX-3AMACRO-29 "TRY:DEFTEST MGL-PAX:MACRO"
  [e80e]: #x-28TRY-3AFAILS-20MGL-PAX-3AMACRO-29 "TRY:FAILS MGL-PAX:MACRO"
  [e8d7]: http://www.lispworks.com/documentation/HyperSpec/Body/f_endp.htm "ENDP (MGL-PAX:CLHS FUNCTION)"
  [e949]: #x-28TRY-3A-2ACATEGORIES-2A-20VARIABLE-29 "TRY:*CATEGORIES* VARIABLE"
  [eb5c]: #x-28TRY-3AON-VALUES-20MGL-PAX-3AMACRO-29 "TRY:ON-VALUES MGL-PAX:MACRO"
  [f001]: #x-28TRY-3ASET-TRY-DEBUG-20FUNCTION-29 "TRY:SET-TRY-DEBUG FUNCTION"
  [f3af]: #x-28TRY-3AIN-TIME-20MGL-PAX-3AMACRO-29 "TRY:IN-TIME MGL-PAX:MACRO"
  [f45a]: #x-28TRY-3ASKIP-TRIAL-20FUNCTION-29 "TRY:SKIP-TRIAL FUNCTION"
  [f472]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm "DEFUN (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [f4bf]: http://www.lispworks.com/documentation/HyperSpec/Body/v_debug_.htm "*QUERY-IO* (MGL-PAX:CLHS VARIABLE)"
  [f58d]: #x-28TRY-3ALEAF-20CONDITION-29 "TRY:LEAF CONDITION"
  [f7f7]: #x-28TRY-3A-40CHECK-LIBRARY-20MGL-PAX-3ASECTION-29 "Check Library"
  [f8bf]: http://www.lispworks.com/documentation/HyperSpec/Body/f_set_di.htm "SET-DIFFERENCE (MGL-PAX:CLHS FUNCTION)"
  [f92d]: #x-28TRY-3AFAILURE-20CONDITION-29 "TRY:FAILURE CONDITION"
  [fae3]: #x-28TRY-3ARETRY-TRIAL-20FUNCTION-29 "TRY:RETRY-TRIAL FUNCTION"
  [fb0e]: #x-28TRY-3ASKIP-CHECK-20FUNCTION-29 "TRY:SKIP-CHECK FUNCTION"
  [fb53]: #x-28TRY-3A-2AIS-CAPTURES-2A-20VARIABLE-29 "TRY:*IS-CAPTURES* VARIABLE"
  [fd3c]: http://www.lispworks.com/documentation/HyperSpec/Body/m_handle.htm "HANDLER-BIND (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [fdf4]: #x-28TRY-3AUNEXPECTED-VERDICT-FAILURE-20CONDITION-29 "TRY:UNEXPECTED-VERDICT-FAILURE CONDITION"
  [ff2c]: #x-28TRY-3A-40CHECKING-CONDITIONS-20MGL-PAX-3ASECTION-29 "Checking Conditions"
  [ffab]: #x-28TRY-3ARESULT-ABORT-2A-20CONDITION-29 "TRY:RESULT-ABORT* CONDITION"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
