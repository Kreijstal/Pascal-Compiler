#!/bin/bash
# Script to test cross-compilation setup using quasi-msys2
# This requires network access to MSYS2 repositories

set -e

echo "=== Testing Cross-Compilation Setup ==="
echo ""

# Check if we're in the project root
if [ ! -f "meson.build" ]; then
    echo "Error: Please run this script from the project root directory"
    exit 1
fi

# Clone quasi-msys2 if not already present
if [ ! -d "quasi-msys2" ]; then
    echo "Cloning quasi-msys2..."
    git clone https://github.com/HolyBlackCat/quasi-msys2.git
fi

cd quasi-msys2

# Set MSYS2 environment (UCRT64 is recommended)
MSYSTEM="${1:-UCRT64}"
echo "$MSYSTEM" > msystem.txt
echo "Using MSYS2 environment: $MSYSTEM"
echo ""

# Install required packages
echo "Installing MSYS2 packages (this may take 10-30 minutes)..."
echo "Packages: gcc, gmp, flex, bison"
make install _gcc _gmp _flex _bison

echo ""
echo "=== Configuring cross-compilation build ==="
bash -c "source env/all.src && cd .. && meson setup builddir-cross --cross-file quasi-msys2/env/meson_cross_file.ini --buildtype=release -Dwith_gmp=enabled"

echo ""
echo "=== Building ==="
bash -c "source env/all.src && cd .. && meson compile -C builddir-cross"

echo ""
echo "=== Checking build outputs ==="
if [ -f ../builddir-cross/GPC/gpc.exe ]; then
    echo "✓ Successfully built gpc.exe"
    ls -lh ../builddir-cross/GPC/gpc.exe
else
    echo "✗ Failed to build gpc.exe"
    exit 1
fi

echo ""
echo "=== Running tests with Wine ==="
bash -c "source env/all.src && cd .. && meson test -C builddir-cross" || {
    echo "Some tests failed, but build succeeded"
    exit 0
}

echo ""
echo "=== Cross-compilation test completed successfully! ==="
