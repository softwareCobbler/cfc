REM todo: generate NodeKind -> int for java interop
mkdir out 2> NUL
npx esbuild src/libcfc/libcfc.ts --bundle --target=esnext --format=iife --global-name=libcfc --outfile=out/libcfc.js
