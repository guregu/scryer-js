const esbuild = require('esbuild');
const fs = require('fs');

(async () => {
	await esbuild.build({
		entryPoints: ['src/scryer.ts'],
		bundle: true,
		outdir: 'dist',
		format: 'esm',
		loader: { '.wasm': 'binary' },
		target: ['es2022'],
		minify: false,
		keepNames: true,
		sourcemap: false,
		plugins: []
	});

	// const src = fs.readFileSync("src/pkg/scryer_prolog.d.ts").toString();
	// const ours = fs.readFileSync("dist/index.d.ts").toString();
	// const fixed = ours.replace("export * from './pkg/scryer_prolog.js';", src + "\n");
	// fs.writeFileSync("dist/index.d.ts", fixed);
	// console.log(fixed);
})();
