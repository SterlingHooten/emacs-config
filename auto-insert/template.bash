#! /bin/bash

PROG=${0##*/}
USAGE="\
$PROG: description of this script.
More description.

Usage:

  $PROG [options] arguments ...
"

while getopts "h" OPT; do
    case "$OPT" in
        h|help)
            echo "$USAGE"
            exit 0
            ;;
        *)
            echo "$USAGE" >&2
            exit 1
            ;;
    esac
done

shift $((OPTIND - 1))

if [[ $# -gt 0 ]]; then
    : do this and that
else
    echo "$USAGE"
    exit 1
fi
