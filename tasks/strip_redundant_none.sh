#!/usr/bin/env bash
# Strip redundant `None` from test tuples in linter rules that take no config.
#
# Only transforms `("source", None)` tuples to bare `"source"` strings within
# `let pass = vec![...]` and `let fail = vec![...]` blocks.
#
# Does NOT touch fix vecs or vecs with mixed types.
#
# Usage:
#   bash tasks/strip_redundant_none.sh          # dry-run (preview)
#   bash tasks/strip_redundant_none.sh --apply  # apply changes

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

RULES_DIR="crates/oxc_linter/src/rules"
MODE="${1:---dry-run}"

# Manually audited: no config, all pass/fail entries are (string, None).
FILES=(
  eslint/no_setter_return.rs
  oxc/bad_comparison_sequence.rs
  oxc/bad_array_method_on_arguments.rs
  oxc/bad_bitwise_operator.rs
  react/no_children_prop.rs
  eslint/no_this_before_super.rs
  eslint/no_var.rs
  eslint/no_empty_character_class.rs
  eslint/no_compare_neg_zero.rs
  typescript/ban_types.rs
  react/jsx_no_duplicate_props.rs
  unicorn/no_instanceof_array.rs
  eslint/no_empty_pattern.rs
  react/no_render_return_value.rs
  oxc/bad_min_max_func.rs
  react/react_in_jsx_scope.rs
  eslint/no_case_declarations.rs
  oxc/uninvoked_array_callback.rs
  oxc/number_arg_out_of_range.rs
  react/no_danger.rs
  oxc/missing_throw.rs
  eslint/prefer_template.rs
  react/jsx_no_comment_textnodes.rs
)

# transform_file FILE
# Outputs transformed content to stdout, replacement count to stderr.
transform_file() {
  local file="$1"
  gawk '
BEGIN {
  in_vec = 0
  depth = 0
  in_tuple = 0
  tuple_lines_n = 0
  count = 0
}

/let (pass|fail) = vec!\[/ {
  in_vec = 1
  depth = 0
  n = split($0, chars, "")
  for (i = 1; i <= n; i++) {
    if (chars[i] == "[") depth++
    if (chars[i] == "]") depth--
  }
  print
  if (depth <= 0) in_vec = 0
  next
}

in_vec {
  n = split($0, chars, "")
  for (i = 1; i <= n; i++) {
    if (chars[i] == "[") depth++
    if (chars[i] == "]") depth--
  }

  if (!in_tuple) {
    # Single-line: ( STRING , None ) possibly followed by , and // comment
    if ($0 ~ /^[[:space:]]*\(/ && $0 ~ /,[[:space:]]*None[[:space:]]*,?[[:space:]]*\)/) {
      line = $0
      match(line, /^[[:space:]]*/)
      indent = substr(line, 1, RLENGTH)

      # Extract trailing comment if present
      comment = ""
      if (line ~ /\/\//) {
        idx = index(line, "//")
        comment = " " substr(line, idx)
      }

      # Strip the tuple wrapper
      sub(/^[[:space:]]*\([[:space:]]*/, "", line)
      # Remove from ", None)" to end of content (before comment)
      sub(/,[[:space:]]*None[[:space:]]*,?[[:space:]]*\)[[:space:]]*,?[[:space:]]*(\/\/.*)?$/, "", line)

      # Determine trailing comma
      trail = ""
      if ($0 ~ /\)[[:space:]]*,/) trail = ","

      print indent line trail comment
      count++
    }
    # Start of multi-line tuple: line with ( but no closing )
    else if ($0 ~ /^[[:space:]]*\([[:space:]]*$/ || ($0 ~ /^[[:space:]]*\(/ && $0 !~ /\)/)) {
      in_tuple = 1
      tuple_lines_n = 1
      tuple_lines[1] = $0
    }
    else {
      print
    }
  }
  else {
    tuple_lines_n++
    tuple_lines[tuple_lines_n] = $0

    # Case 1: None) or None,) on same line
    is_none_close = ($0 ~ /^[[:space:]]*None[[:space:]]*,?[[:space:]]*\)/)

    # Case 2: just ) and previous line was None,
    if (!is_none_close && $0 ~ /^[[:space:]]*\)[[:space:]]*,?[[:space:]]*$/ && tuple_lines_n >= 2) {
      prev = tuple_lines[tuple_lines_n - 1]
      if (prev ~ /^[[:space:]]*None[[:space:]]*,?[[:space:]]*$/) {
        is_none_close = 1
      }
    }

    if (is_none_close) {
      result = ""
      for (j = 1; j <= tuple_lines_n; j++) {
        tl = tuple_lines[j]
        # Skip None lines and ) lines
        if (tl ~ /^[[:space:]]*None[[:space:]]*,?[[:space:]]*\)/) continue
        if (tl ~ /^[[:space:]]*None[[:space:]]*,?[[:space:]]*$/) continue
        if (tl ~ /^[[:space:]]*\)[[:space:]]*,?[[:space:]]*$/) continue
        # Remove opening ( from first line
        if (j == 1) sub(/\([[:space:]]*/, "", tl)
        # Remove trailing comma before None
        if (j < tuple_lines_n) {
          nxt = tuple_lines[j + 1]
          if (nxt ~ /^[[:space:]]*None/) sub(/,[[:space:]]*$/, "", tl)
        }
        # Skip if empty after stripping
        if (tl ~ /^[[:space:]]*$/) continue
        if (result != "") result = result "\n"
        result = result tl
      }
      # Trailing comma if original tuple had one
      last_tl = tuple_lines[tuple_lines_n]
      if (last_tl ~ /\)[[:space:]]*,/) result = result ","
      print result
      in_tuple = 0
      tuple_lines_n = 0
      count++
    }
    # Closes with ) but not a None tuple — print unchanged
    else if ($0 ~ /^[[:space:]]*\)[[:space:]]*,?[[:space:]]*$/) {
      for (j = 1; j <= tuple_lines_n; j++) print tuple_lines[j]
      in_tuple = 0
      tuple_lines_n = 0
    }
  }

  if (depth <= 0) {
    in_vec = 0
    if (in_tuple) {
      for (j = 1; j <= tuple_lines_n; j++) print tuple_lines[j]
      in_tuple = 0
      tuple_lines_n = 0
    }
  }
  next
}

{ print }

END {
  printf "%d", count > "/dev/stderr"
}
' "$file"
}

changed=0
skipped=0

for rel in "${FILES[@]}"; do
  filepath="$RULES_DIR/$rel"

  if [[ ! -f "$filepath" ]]; then
    echo "SKIP (not found): $rel"
    ((skipped++)) || true
    continue
  fi

  if [[ "$MODE" == "--apply" ]]; then
    tmpfile=$(mktemp)
    count=$(transform_file "$filepath" 2>&1 >"$tmpfile")

    if [[ "$count" -eq 0 ]]; then
      rm -f "$tmpfile"
      echo "SKIP (no matches): $rel"
      ((skipped++)) || true
      continue
    fi

    mv "$tmpfile" "$filepath"
    echo "UPDATED ($count replacements): $rel"
    ((changed++)) || true
  else
    count=$(transform_file "$filepath" 2>&1 >/dev/null)

    if [[ "$count" -eq 0 ]]; then
      echo "SKIP (no matches): $rel"
      ((skipped++)) || true
      continue
    fi

    echo "WOULD UPDATE ($count replacements): $rel"
    ((changed++)) || true
  fi
done

echo ""
if [[ "$MODE" == "--apply" ]]; then
  echo "Updated $changed files, skipped $skipped"
  echo ""
  echo "Next steps:"
  echo "  cargo fmt                 # re-format modified files"
  echo "  cargo test -p oxc_linter  # verify tests still pass"
else
  echo "Dry run complete: $changed files would change, $skipped skipped"
  echo "Re-run with --apply to make changes"
fi
