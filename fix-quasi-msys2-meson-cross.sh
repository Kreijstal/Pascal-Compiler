#!/bin/bash
# Fix quasi-msys2 meson cross file to include explicit compiler definitions
# This avoids dependency on wrapper script environment variables

set -e

if [ ! -d "quasi-msys2" ]; then
    echo "Error: quasi-msys2 directory not found"
    exit 1
fi

echo "Applying patch to quasi-msys2/env/generate_meson_config.mk..."

cd quasi-msys2

# Create a backup
cp env/generate_meson_config.mk env/generate_meson_config.mk.bak

# Apply the patch
patch -p1 << 'EOF'
--- a/env/generate_meson_config.mk
+++ b/env/generate_meson_config.mk
@@ -25,8 +25,11 @@ pkg-config = 'pkg-config'
 cmake = 'cmake'
 strip = 'strip'
 windres = 'windres'
-$(if $(AR),ar = '$(AR)',# ar = ??)
-$(if $(RANLIB),ranlib = '$(RANLIB)',# ranlib = ??)
+$(if $(WIN_NATIVE_CLANG_AR),ar = '$(WIN_NATIVE_CLANG_AR)',# ar = ??)
+$(if $(WIN_NATIVE_CLANG_RANLIB),ranlib = '$(WIN_NATIVE_CLANG_RANLIB)',# ranlib = ??)
+# Specify C and C++ compilers with their flags directly to avoid wrapper script dependency on environment variables
+$(if $(WIN_NATIVE_CLANG_CC),c = ['$(WIN_NATIVE_CLANG_CC)'$(shell echo "$(WIN_NATIVE_CLANG_FLAGS)" | awk '{for(i=1;i<=NF;i++) printf ", '\''%s'\''", $$i}')],# c = ??)
+$(if $(WIN_NATIVE_CLANG_CXX),cpp = ['$(WIN_NATIVE_CLANG_CXX)'$(shell echo "$(WIN_NATIVE_CLANG_FLAGS)" | awk '{for(i=1;i<=NF;i++) printf ", '\''%s'\''", $$i}')],# cpp = ??)
 
 [host_machine]
 system = 'windows'
EOF

cd ..

echo "Patch applied successfully!"
echo "The meson cross file will now be regenerated with explicit compiler definitions when you source env/all.src"
