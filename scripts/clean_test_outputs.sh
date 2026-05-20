#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: scripts/clean_test_outputs.sh [--yes] [--keep-bootstrap]

Removes generated local test artifacts:
  - tests/output/
  - failing-tests/
  - meson-logs/

By default this is a dry run. Pass --yes to delete files.

Options:
  --yes             Perform deletion. Without this, only print what would be removed.
  --keep-bootstrap Keep tests/output/pp_bootstrap, pp_bootstrap.s, and pp_stage2/.
  -h, --help        Show this help.
EOF
}

dry_run=1
keep_bootstrap=0

while [ "$#" -gt 0 ]; do
  case "$1" in
    --yes)
      dry_run=0
      ;;
    --keep-bootstrap)
      keep_bootstrap=1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"

remove_path() {
  local path=$1
  if [ ! -e "$path" ]; then
    return
  fi

  if [ "$dry_run" -eq 1 ]; then
    du -sh "$path" 2>/dev/null || true
    return
  fi

  rm -rf -- "$path"
}

remove_tests_output_contents() {
  local output_dir="tests/output"
  if [ ! -d "$output_dir" ]; then
    return
  fi

  if [ "$keep_bootstrap" -eq 0 ]; then
    remove_path "$output_dir"
    return
  fi

  if [ "$dry_run" -eq 1 ]; then
    find "$output_dir" -mindepth 1 \
      ! -path "$output_dir/pp_bootstrap" \
      ! -path "$output_dir/pp_bootstrap.s" \
      ! -path "$output_dir/pp_stage2" \
      ! -path "$output_dir/pp_stage2/*" \
      -print | sed 's/^/would remove /'
    return
  fi

  find "$output_dir" -mindepth 1 \
    ! -path "$output_dir/pp_bootstrap" \
    ! -path "$output_dir/pp_bootstrap.s" \
    ! -path "$output_dir/pp_stage2" \
    ! -path "$output_dir/pp_stage2/*" \
    -exec rm -rf -- {} +
}

if [ "$dry_run" -eq 1 ]; then
  echo "Dry run. Pass --yes to delete generated outputs."
else
  echo "Deleting generated outputs."
fi

remove_tests_output_contents
remove_path "failing-tests"
remove_path "meson-logs"

if [ "$dry_run" -eq 1 ]; then
  echo "Dry run complete."
else
  echo "Cleanup complete."
fi
