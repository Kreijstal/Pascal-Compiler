# Cross-Compilation Testing

## Overview

This directory contains tools and workflows for testing cross-compilation from Linux to Windows using Wine and quasi-msys2.

## Quick Start

To test cross-compilation locally:

```bash
./test-cross-compile.sh [UCRT64|MINGW64]
```

This script will:
1. Clone quasi-msys2 if not present
2. Install required MSYS2 packages (GCC, GMP, flex, bison)
3. Configure the build with Meson cross-compilation settings
4. Build the Windows executable
5. Run tests using Wine

**Note:** This requires network access to MSYS2 package repositories.

## CI/CD Testing

The `.github/workflows/cross-compile-ci.yml` workflow automatically tests cross-compilation in GitHub Actions, which has full network access.

## Manual Testing Steps

If the automated script doesn't work, follow these manual steps:

### 1. Install Dependencies

```bash
# Ubuntu/Debian
sudo apt-get install -y make wget tar zstd gawk gpg gpgv wine wine64
sudo apt-get install -y llvm clang lld flex bison
pip install meson ninja
```

### 2. Set Up quasi-msys2

```bash
git clone https://github.com/HolyBlackCat/quasi-msys2.git
cd quasi-msys2
echo "UCRT64" > msystem.txt  # or MINGW64 for older Windows
```

### 3. Install MSYS2 Packages

```bash
# This downloads Windows libraries and toolchain
make install _gcc _gmp _flex _bison
```

This step may take 10-30 minutes depending on your network speed.

### 4. Configure Build

```bash
cd ..
bash -c 'source quasi-msys2/env/all.src && meson setup builddir-cross --cross-file quasi-msys2/env/meson_cross_file.ini --buildtype=release -Dwith_gmp=enabled'
```

### 5. Build

```bash
bash -c 'source quasi-msys2/env/all.src && meson compile -C builddir-cross'
```

### 6. Verify Output

```bash
ls -lh builddir-cross/GPC/gpc.exe
file builddir-cross/GPC/gpc.exe
```

Should show a Windows PE executable.

### 7. Run Tests (with Wine)

```bash
bash -c 'source quasi-msys2/env/all.src && meson test -C builddir-cross'
```

## Troubleshooting

### Network Issues

If you see "Unable to download the database signature" or "No address associated with hostname":
- Check your internet connection
- Try a different network (some corporate networks block MSYS2 repos)
- The GitHub Actions CI environment should work fine

### Wine Issues

If Wine isn't working:
```bash
wine --version  # Check Wine is installed
winecfg         # Configure Wine if needed
```

### Build Failures

Check the meson logs:
```bash
cat builddir-cross/meson-logs/meson-log.txt
```

## MSYS2 Environments

quasi-msys2 supports multiple MSYS2 environments:

- **UCRT64** (recommended): Universal C Runtime, modern Windows 10+
- **MINGW64**: Traditional MinGW, works on older Windows
- **CLANG64**: Uses Clang instead of GCC
- **MINGW32**: 32-bit (not recommended for this project)

Change environment by modifying `quasi-msys2/msystem.txt` before installing packages.

## Related Files

- `.github/workflows/cross-compile-ci.yml` - GitHub Actions workflow for CI
- `.github/copilot-instructions.md` - Full build documentation
- `test-cross-compile.sh` - Automated test script
