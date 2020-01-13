#lang scribble/manual

@require[@for-label[zui/notify-changes
                    racket/base]]

@title{zui/notify-changes}
@author{nikitazu}

@defmodule[zui/notify-changes]

@section[#:tag "zui/notify-changes"]{Notify Changes}

This module provides ``notify-changes-mixin'' mixin,
which allows to create an object and subscribe on
changes to its properties.
