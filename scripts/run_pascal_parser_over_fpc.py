#!/usr/bin/env python3
"""Batch runner for pascal_parser_cli against all .pas files in the sysrpl/fpc repo.

The script keeps a JSONL cache so it can be resumed without re-parsing files.
"""
from __future__ import annotations

import argparse
import json
import subprocess
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Dict, Iterable, List, Tuple

DEFAULT_TIMEOUT = 8
DEFAULT_WORKERS = 32
RESULTS_DIR = Path('analysis')
JSONL_FILE = RESULTS_DIR / 'fpc_pascal_parser_results.jsonl'
SUMMARY_FILE = RESULTS_DIR / 'fpc_pascal_parser_summary.json'


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fpc-root', type=Path, default=Path('../fpc'),
                        help='Path to the sysrpl/fpc checkout (default: ../fpc)')
    parser.add_argument('--parser-cli', type=Path, default=Path('builddir/pascal_parser_cli'),
                        help='Path to pascal_parser_cli executable')
    parser.add_argument('--timeout', type=float, default=DEFAULT_TIMEOUT,
                        help=f'Per-file timeout in seconds (default: {DEFAULT_TIMEOUT})')
    parser.add_argument('--workers', type=int, default=DEFAULT_WORKERS,
                        help=f'Number of parallel workers (default: {DEFAULT_WORKERS})')
    parser.add_argument('--resume', action='store_true',
                        help='Resume from existing JSONL results (default behaviour)')
    parser.add_argument('--no-resume', dest='resume', action='store_false',
                        help='Ignore cached results and re-parse everything')
    parser.set_defaults(resume=True)
    parser.add_argument('--limit', type=int, default=None,
                        help='Limit number of files processed (for debugging)')
    return parser.parse_args()


def load_existing_results() -> Dict[str, dict]:
    if not JSONL_FILE.exists():
        return {}
    existing: Dict[str, dict] = {}
    with JSONL_FILE.open('r', encoding='utf-8') as infile:
        for line in infile:
            line = line.strip()
            if not line:
                continue
            record = json.loads(line)
            existing[record['file']] = record
    return existing


def gather_pas_files(fpc_root: Path) -> List[Tuple[int, Path]]:
    return sorted((path.stat().st_size, path)
                  for path in fpc_root.rglob('*.pas'))


def run_pascal_parser(cli: Path, fpc_root: Path, timeout: float, size_path: Tuple[int, Path]) -> dict:
    size, path = size_path
    rel = path.relative_to(fpc_root)
    try:
        proc = subprocess.run(
            [str(cli), str(path)],
            capture_output=True,
            text=True,
            timeout=timeout,
            encoding='utf-8',
            errors='replace',
        )
    except subprocess.TimeoutExpired as exc:
        stdout = (exc.stdout or '').strip()
        if stdout:
            stdout = '\n'.join(stdout.splitlines()[-20:])[-4000:]
        return {
            'file': str(rel).replace('\\', '/'),
            'size': size,
            'success': False,
            'reason': 'Timeout',
            'stdout': (stdout + '\n[TIMEOUT]') if stdout else '[TIMEOUT]'
        }

    success = proc.returncode == 0
    stdout = proc.stdout.strip()
    reason = None
    if not success:
        lines = stdout.splitlines()
        for line in reversed(lines):
            if 'Error at line' in line:
                reason = line.strip()
                break
        if reason is None and lines:
            reason = lines[-1].strip()
        if not reason:
            reason = 'Unknown error'
    truncated = '\n'.join(stdout.splitlines()[-20:])[-4000:]
    return {
        'file': str(rel).replace('\\', '/'),
        'size': size,
        'success': success,
        'reason': reason,
        'stdout': truncated,
    }


def write_summary(results: Iterable[dict]) -> None:
    total = success = 0
    failures = 0
    fail_reasons: Counter[str] = Counter()
    for record in results:
        total += 1
        if record['success']:
            success += 1
        else:
            failures += 1
            fail_reasons[record.get('reason') or 'Unknown error'] += 1
    SUMMARY_FILE.write_text(json.dumps({
        'total': total,
        'success': success,
        'failure': failures,
        'fail_reasons': fail_reasons.most_common(100),
    }, indent=2), encoding='utf-8')


def main() -> None:
    global args
    args = parse_args()
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)

    cli = args.parser_cli
    if not cli.exists():
        raise SystemExit(f'pascal_parser_cli not found at {cli}. Build it first.')

    files = gather_pas_files(args.fpc_root)
    if args.limit is not None:
        files = files[:args.limit]

    existing: Dict[str, dict] = {}
    if args.resume:
        existing = load_existing_results()

    pending = [item for item in files if str(item[1].relative_to(args.fpc_root)).replace('\\', '/') not in existing]
    total_files = len(files)
    print(f'Total Pascal files discovered: {total_files}')
    if existing:
        print(f'Resuming with {len(existing)} existing results. Pending: {len(pending)}')
    else:
        print(f'Pending files to parse: {len(pending)}')

    if not pending:
        print('No pending files to process. Regenerating summary only.')
        write_summary(existing.values())
        return

    with ThreadPoolExecutor(max_workers=args.workers) as executor, JSONL_FILE.open('a', encoding='utf-8') as outfile:
        future_map = {executor.submit(run_pascal_parser, args.parser_cli, args.fpc_root, args.timeout, item): item for item in pending}
        completed = 0
        for future in as_completed(future_map):
            record = future.result()
            outfile.write(json.dumps(record) + '\n')
            outfile.flush()
            existing[record['file']] = record
            completed += 1
            if completed % 50 == 0 or completed == len(pending):
                success_count = sum(1 for r in existing.values() if r['success'])
                failure_count = len(existing) - success_count
                timeout_count = sum(1 for r in existing.values() if r.get('reason') == 'Timeout')
                print(f'Progress: {len(existing)}/{total_files} processed | '
                      f'success {success_count} | failure {failure_count} | timeouts {timeout_count}')

    write_summary(existing.values())
    print('Summary written to', SUMMARY_FILE)


if __name__ == '__main__':
    main()
