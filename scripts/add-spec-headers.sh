#!/usr/bin/env bash
# Add standardized Emacs Lisp file headers with "Specifications:" list.
# This script is intended to be run from the repository root.
#
# It will prepend (or inject) a canonical Emacs/MELPA-style header into
# target .el files that do not already contain a "Specifications:" section.
#
# Usage:
#   ./scripts/add-spec-headers.sh
#
# Notes:
# - Headers are English-only, conservative, and safe to apply repeatedly
#   (idempotent: if a file already contains "Specifications:" it is skipped).
# - The mapping below contains per-file spec lists (relative paths under spec/).
# - The script only modifies files that exist; missing files are reported.
# - Review changes with `git diff` before committing.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd -P)"
cd "$REPO_ROOT"

# Mandatory specs for all code files
MANDATORY_SPECS=(
  "spec/code-style-v1.org"
  "spec/index.org"
  "spec/errors-v1.org"
  "spec/compliance-checklist-v1.org"
)

# Per-file extra specs (relative to repo root).
# Add or adjust entries as needed.
declare -A EXTRA_SPECS
EXTRA_SPECS["lisp/carriage.el"]="spec/carriage-mode-v1.org spec/apply-engines-v1.org spec/apply-pipeline-v1.org spec/async-workflow-v1.org spec/llm-transport-v1.org spec/extensibility-points-v1.org spec/data-structures-v1.org spec/git-integration-v1.org"
EXTRA_SPECS["lisp/carriage-utils.el"]="spec/git-integration-v1.org spec/async-workflow-v1.org spec/security-v1.org"
EXTRA_SPECS["lisp/carriage-git.el"]="spec/git-integration-v1.org spec/apply-engines-v1.org spec/apply-pipeline-v1.org spec/security-v1.org"
EXTRA_SPECS["lisp/carriage-apply.el"]="spec/apply-pipeline-v1.org spec/apply-engines-v1.org spec/async-workflow-v1.org spec/patch-unified-diff-v1.org"
EXTRA_SPECS["lisp/carriage-format-registry.el"]="spec/parser-registry-v1.org spec/parser-impl-v1.org"
EXTRA_SPECS["lisp/carriage-parser.el"]="spec/parser-registry-v1.org spec/parser-impl-v1.org spec/patch-unified-diff-v1.org spec/sre-v1.org"
EXTRA_SPECS["lisp/carriage-sre-core.el"]="spec/sre-v1.org spec/parser-impl-v1.org"
EXTRA_SPECS["lisp/ops/carriage-op-sre.el"]="spec/sre-v1.org spec/parser-impl-v1.org"
EXTRA_SPECS["lisp/ops/carriage-op-aibo.el"]="spec/aibo-v1.org spec/sre-v1.org"
EXTRA_SPECS["lisp/ops/carriage-op-patch.el"]="spec/patch-unified-diff-v1.org spec/parser-impl-v1.org"
EXTRA_SPECS["lisp/ops/carriage-op-file.el"]="spec/file-ops-v1.org spec/security-v1.org"
EXTRA_SPECS["lisp/carriage-report.el"]="spec/ui-v1.org spec/apply-pipeline-v1.org"
EXTRA_SPECS["lisp/carriage-ui.el"]="spec/ui-v1.org spec/keyspec-v1.org spec/i18n-v1.org spec/context-integration-v1.org"
EXTRA_SPECS["lisp/carriage-context.el"]="spec/context-integration-v1.org spec/testing-v1.org"
EXTRA_SPECS["lisp/carriage-traffic-batch.el"]="spec/llm-transport-v1.org spec/logging-v1.org spec/testing-v1.org"
EXTRA_SPECS["lisp/carriage-task.el"]="spec/task-docs-v1.org spec/keyspec-v1.org spec/context-integration-v1.org"
EXTRA_SPECS["lisp/carriage-intent-registry.el"]="spec/prompt-profiles-v1.org spec/llm-transport-v1.org"
EXTRA_SPECS["lisp/carriage-llm-registry.el"]="spec/llm-transport-v1.org spec/prompt-profiles-v1.org"
EXTRA_SPECS["lisp/carriage-keyspec.el"]="spec/keyspec-v1.org spec/ui-v1.org"
EXTRA_SPECS["lisp/carriage-global-mode.el"]="spec/ui-v1.org spec/keyspec-v1.org"
EXTRA_SPECS["lisp/carriage-i18n.el"]="spec/i18n-v1.org"
EXTRA_SPECS["lisp/carriage-announce.el"]="spec/logging-v1.org spec/apply-pipeline-v1.org"
EXTRA_SPECS["lisp/carriage-clean.el"]="spec/ui-v1.org spec/parser-impl-v1.org"
EXTRA_SPECS["lisp/carriage-errors.el"]="spec/errors-v1.org"
EXTRA_SPECS["lisp/carriage-format-registry.el"]="spec/parser-registry-v1.org"
EXTRA_SPECS["lisp/carriage-iteration.el"]="spec/iteration-markers-v1.org spec/iteration-markers-placement-v1.org"
EXTRA_SPECS["lisp/carriage-suite.el"]="spec/prompt-profiles-v1.org spec/extensibility-points-v1.org"
EXTRA_SPECS["lisp/carriage-logging.el"]="spec/logging-v1.org spec/testing-v1.org"
EXTRA_SPECS["lisp/carriage-mode.el"]="spec/carriage-mode-v1.org spec/ui-v1.org spec/apply-pipeline-v1.org"
EXTRA_SPECS["lisp/carriage-keyspec.el"]="spec/keyspec-v1.org spec/ui-v1.org"
EXTRA_SPECS["lisp/engines/carriage-apply-engine.el"]="spec/apply-engines-v1.org"
EXTRA_SPECS["lisp/engines/carriage-engine-emacs.el"]="spec/emacs-engine-udiff-design-v1.org spec/emacs-engine-udiff-impl-plan-v1.org"
EXTRA_SPECS["lisp/engines/carriage-engine-git.el"]="spec/apply-engines-v1.org spec/git-integration-v1.org"
EXTRA_SPECS["lisp/transports/carriage-transport.el"]="spec/llm-transport-v1.org spec/logging-v1.org"
EXTRA_SPECS["lisp/transports/carriage-transport-gptel.el"]="spec/llm-transport-v1.org"
EXTRA_SPECS["lisp/transports/carriage-transport-echo.el"]="spec/llm-transport-v1.org"
EXTRA_SPECS["lisp/profile-carriage.el"]="spec/testing-v1.org spec/async-workflow-v1.org"
EXTRA_SPECS["lisp/carriage-clean.el"]="spec/ui-v1.org"
EXTRA_SPECS["lisp/carriage-errors.el"]="spec/errors-v1.org"

# Files to process (subset: all .el under lisp/ and engines/ops/transports as listed)
FILES=(
  "lisp/carriage-announce.el"
  "lisp/carriage-apply.el"
  "lisp/carriage-clean.el"
  "lisp/carriage-context.el"
  "lisp/carriage-errors.el"
  "lisp/carriage-format-registry.el"
  "lisp/carriage-git.el"
  "lisp/carriage-global-mode.el"
  "lisp/carriage-i18n.el"
  "lisp/carriage-intent-registry.el"
  "lisp/carriage-iteration.el"
  "lisp/carriage-keyspec.el"
  "lisp/carriage-llm-registry.el"
  "lisp/carriage-logging.el"
  "lisp/carriage-mode.el"
  "lisp/carriage-parser.el"
  "lisp/carriage-report.el"
  "lisp/carriage-sre-core.el"
  "lisp/carriage-suite.el"
  "lisp/carriage-task.el"
  "lisp/carriage-traffic-batch.el"
  "lisp/carriage-ui.el"
  "lisp/carriage-utils.el"
  "lisp/carriage.el"
  "lisp/engines/carriage-apply-engine.el"
  "lisp/engines/carriage-engine-emacs.el"
  "lisp/engines/carriage-engine-git.el"
  "lisp/ops/carriage-op-aibo.el"
  "lisp/ops/carriage-op-file.el"
  "lisp/ops/carriage-op-patch.el"
  "lisp/ops/carriage-op-sre.el"
  "lisp/profile-carriage.el"
  "lisp/transports/carriage-transport-echo.el"
  "lisp/transports/carriage-transport-gptel.el"
  "lisp/transports/carriage-transport.el"
)

# Template header generator
generate_header() {
  local filepath="$1"
  local rel="$filepath"
  local name="$(basename "$filepath")"
  # Build spec list: mandatory + extras for this file
  local specs=()
  for s in "${MANDATORY_SPECS[@]}"; do specs+=("$s"); done
  if [[ -n "${EXTRA_SPECS[$rel]:-}" ]]; then
    for s in ${EXTRA_SPECS[$rel]}; do
      # Avoid duplicates
      local skip=0
      for ex in "${specs[@]}"; do [[ "$ex" == "$s" ]] && skip=1 && break; done
      (( skip == 0 )) && specs+=("$s")
    done
  fi

  # Build header text (Emacs Lisp style)
  {
    printf ";;; %s --- Brief description of %s  -*- lexical-binding: t; -*-\n" "$name" "$name"
    printf ";;\n"
    printf ";; Copyright (C) %s Your Name\n" "$(date +%Y)"
    printf ";; Author: Carriage Team\n"
    printf ";; URL: https://example.org/carriage\n"
    printf ";; Package-Requires: ((emacs \"27.1\"))\n"
    printf ";; Version: 0.1\n"
    printf ";; Keywords: tools, convenience\n"
    printf ";;\n"
    printf ";; Specifications:\n"
    for s in "${specs[@]}"; do printf ";;   %s\n" "$s"; done
    printf ";;\n"
    printf ";;; Commentary:\n"
    printf ";; %s\n" "High-level purpose and relationship to specifications listed above."
    printf ";;\n"
    printf ";;; Code:\n\n"
  } 
}

# Process each file
modified=0
for f in "${FILES[@]}"; do
  if [[ ! -f "$f" ]]; then
    echo "SKIP: not found: $f"
    continue
  fi

  # Detect existing Specifications: block
  if grep -qE "^;;[[:space:]]*Specifications:" "$f"; then
    echo "SKIP: already has Specifications: $f"
    continue
  fi

  echo "Patching: $f"

  tmp="$(mktemp)"
  # Generate header and then append original content
  generate_header "$f" > "$tmp"
  # If original file already starts with a shebang or -*- mode lines, we still prepend header.
  cat "$f" >> "$tmp"
  # Ensure file permissions preserved
  chmod --reference="$f" "$tmp" || true
  mv "$tmp" "$f"
  git add -- "$f" || true
  modified=$((modified+1))
done

echo "Patching complete. Files modified: $modified"
echo "Note: script adds headers only when 'Specifications:' is not found."
echo "Review changes with: git --no-pager diff --staged"
exit 0
