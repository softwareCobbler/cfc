mkdir out 2> NUL
npx esbuild src/libcfc/libcfc.ts --bundle --target=esnext --format=esm --outfile=out/libcfc.js
