#!/bin/bash
set -e

BUILD_DIR="build"
KGPC="${BUILD_DIR}/KGPC/kgpc"
AST_CACHE_DIR="test_ast_cache"
CODEGEN_CACHE_DIR="test_codegen_cache"
INPUT="tests/test_cases/abi_array_param_bug.p"

mkdir -p ${AST_CACHE_DIR} ${CODEGEN_CACHE_DIR}
rm -rf ${AST_CACHE_DIR}/* ${CODEGEN_CACHE_DIR}/*

echo "Testing parallel AST and codegen cache population..."

# Run 10 instances in parallel
PIDS=()
for i in {1..10}; do
    ${KGPC} ${INPUT} "parallel_test_$i.s" \
        --pp-cache-dir=${AST_CACHE_DIR} \
        --codegen-cache-dir=${CODEGEN_CACHE_DIR} > "parallel_test_$i.log" 2>&1 &
    PIDS+=($!)
done

# Wait for all to finish
for pid in "${PIDS[@]}"; do
    wait $pid
done

# Check logs
HITS=$(grep -h "AST cache load success" parallel_test_*.log | wc -l || true)
MISSES=$(grep -h "AST cache miss" parallel_test_*.log | wc -l || true)
CODEGEN_HITS=$(grep -h "Codegen cache hit" parallel_test_*.log | wc -l || true)
CODEGEN_MISSES=$(grep -h "Codegen cache miss" parallel_test_*.log | wc -l || true)

echo "AST Cache Loads: $HITS"
echo "Codegen Cache Hits: $CODEGEN_HITS"
echo "Codegen Cache Misses: $CODEGEN_MISSES"

# Cleanup
rm parallel_test_*.log parallel_test_*.s
rm -rf ${AST_CACHE_DIR} ${CODEGEN_CACHE_DIR}

if [ "$CODEGEN_MISSES" -gt 1 ]; then
    echo "FAILURE: More than one process ($CODEGEN_MISSES) reported a codegen cache miss!"
    exit 1
else
    echo "SUCCESS: Parallel locking seems to work."
fi
