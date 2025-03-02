import { build } from "esbuild";

await build({
  entryPoints: ["src/scryer.ts"],
  bundle: true,
  outdir: "dist",
  format: "esm",
  loader: { ".wasm": "binary" },
  target: ["es2022"],
  minify: false,
  keepNames: true,
  sourcemap: true,
  plugins: [],
});
