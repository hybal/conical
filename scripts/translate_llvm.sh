#!/bin/bash

LLVM_INCLUDE_DIR=$(llvm-config --includedir)
LLVM_HEADERS=($LLVM_INCLUDE_DIR/llvm-c/*.h)
echo "" > src/llvm-c/mod.zig
for file in "${LLVM_HEADERS[@]}"; do
    echo "Translating File '$file'..."
    filename=$(basename $file)
    zig translate-c -I$LLVM_INCLUDE_DIR $file > src/llvm-c/${filename%.h}.zig
    echo "pub const ${filename%.*} = @import(\"${filename%.h}.zig\");" >> src/llvm-c/mod.zig
done
echo "Done!"

