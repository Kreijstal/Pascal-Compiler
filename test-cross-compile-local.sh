#!/bin/bash
# Local test simulation of .github/workflows/cross-compile-ci.yml
# This attempts to run the workflow steps locally to validate the configuration

set -e

MSYSTEM="${1:-UCRT64}"
echo "=== Testing Cross-Compilation CI Workflow Locally ==="
echo "MSYSTEM: $MSYSTEM"
echo ""

# Step 1: Check repository
echo "Step 1: Check repository"
if [ ! -f "meson.build" ]; then
    echo "Error: Not in repository root"
    exit 1
fi
echo "✓ Repository checked out"
echo ""

# Step 2: Check Python
echo "Step 2: Check Python version"
python3 --version
echo "✓ Python available"
echo ""

# Step 3: Install dependencies (check only, not installing)
echo "Step 3: Checking dependencies"
dependencies=(make wget tar zstd gawk gpg gpgv wine llvm clang lld flex bison meson ninja)
missing=()
for dep in "${dependencies[@]}"; do
    if ! command -v "$dep" &> /dev/null; then
        missing+=("$dep")
    fi
done

if [ ${#missing[@]} -gt 0 ]; then
    echo "⚠ Missing dependencies: ${missing[*]}"
    echo "Run: sudo apt-get install -y make wget tar zstd gawk gpg gpgv wine wine64 flex bison llvm clang lld"
    echo "     pip install meson ninja"
else
    echo "✓ All dependencies available"
fi
echo ""

# Step 4: Clone quasi-msys2
echo "Step 4: Clone quasi-msys2"
if [ ! -d "quasi-msys2" ]; then
    echo "Cloning quasi-msys2..."
    git clone https://github.com/HolyBlackCat/quasi-msys2.git
fi
cd quasi-msys2
echo "$MSYSTEM" > msystem.txt
echo "✓ quasi-msys2 cloned and configured"
echo ""

# Step 5: Install MSYS2 packages (attempt, may fail due to network)
echo "Step 5: Install MSYS2 packages"
echo "Attempting: make install _gcc _gmp _meson _ninja _python"
if timeout 60 make install _gcc _gmp _meson _ninja _python 2>&1 | tee /tmp/package-install.log; then
    echo "✓ MSYS2 packages installed"
else
    exitcode=$?
    if [ $exitcode -eq 124 ]; then
        echo "⚠ Package installation timed out (likely network issue)"
    else
        echo "⚠ Package installation failed (exit code: $exitcode)"
        echo "This is expected if MSYS2 repos are not accessible"
        tail -20 /tmp/package-install.log
    fi
fi
echo ""

# Step 6: Configure build environment
echo "Step 6: Configure build environment"
if [ -f "env/vars.src" ]; then
    bash -c 'source env/vars.src && env | grep -E "^(CC|CXX|AR|RANLIB|PKG_CONFIG|PATH|MSYSTEM)" > /tmp/cross_env.txt' || true
    if [ -f "/tmp/cross_env.txt" ]; then
        echo "Environment variables:"
        cat /tmp/cross_env.txt
        echo "✓ Build environment configured"
    else
        echo "⚠ Could not configure build environment"
    fi
else
    echo "⚠ env/vars.src not found (packages may not be installed)"
fi
echo ""

# Step 7: Configure build with Meson
echo "Step 7: Configure build with Meson"
if bash -c 'source env/all.src && cd .. && meson setup builddir-cross' 2>&1 | tee /tmp/meson-setup.log; then
    echo "✓ Meson configuration successful"
else
    echo "⚠ Meson configuration failed"
    tail -20 /tmp/meson-setup.log
fi
echo ""

# Step 8: Build
echo "Step 8: Build"
if [ -d "../builddir-cross" ]; then
    if bash -c 'source env/all.src && cd .. && meson compile -C builddir-cross' 2>&1 | tee /tmp/meson-compile.log; then
        echo "✓ Build successful"
    else
        echo "⚠ Build failed"
        tail -20 /tmp/meson-compile.log
    fi
else
    echo "⚠ Build directory not created (configuration may have failed)"
fi
echo ""

# Step 9: Check build outputs
echo "Step 9: Check build outputs"
cd ..
if [ -d "builddir-cross" ]; then
    echo "Files in builddir-cross/KGPC:"
    ls -lh builddir-cross/KGPC/ 2>/dev/null || echo "KGPC directory not found"
    
    echo ""
    echo "Searching for executables:"
    find builddir-cross -name "*.exe" -o -name "kgpc" 2>/dev/null | head -20 || echo "No executables found"
else
    echo "⚠ Build directory not created"
fi
echo ""

# Step 10: Summary
echo "================================"
echo "Workflow Test Summary"
echo "================================"
echo "Workflow file: .github/workflows/cross-compile-ci.yml"
echo "Test result: Check the output above for each step"
echo ""
echo "Note: Full testing requires network access to MSYS2 repositories."
echo "This test validates the workflow structure and dependencies."
