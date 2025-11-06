#!/usr/bin/env sh
# Run the ERT test that exercises "Apply last iteration" in batch mode.
# Usage:
#   ./scripts/run-apply-all-ert.sh /abs/project/root /abs/path/to/file.org /abs/path/to/logdir
set -eu

PROJECT="${1:-}"
ORGFILE="${2:-}"
LOGDIR="${3:-}"

if [ -z "${PROJECT}" ] || [ -z "${ORGFILE}" ]; then
  echo "Usage: $0 /abs/project/root /abs/path/to/file.org /abs/path/to/logdir" >&2
  exit 2
fi

: "${LOGDIR:=${HOME}/tmp/carriage-debug}"

export CARRIAGE_TEST_PROJECT="${PROJECT}"
export CARRIAGE_TEST_ORG="${ORGFILE}"
export CARRIAGE_TEST_LOGDIR="${LOGDIR}"

exec emacs -Q --batch \
  -l ert \
  -l /home/az/Code/carriage/test/ert/carriage-apply-all-nocrash-tests.el \
  -f ert-run-tests-batch-and-exit
