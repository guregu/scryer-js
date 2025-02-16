import test from "node:test";
import assert from "node:assert";

import { init, Prolog } from "./dist/scryer.js";

await test("load", async (t) => {
	await init();
});

test("query", async (t) => {
	const pl = new Prolog();
	const query = pl.query("X = 1 ; X = 2 ; X = hello.");
	const want = [
		{ type: 'integer', integer: 1n },
		{ type: 'integer', integer: 2n },
		{ type: 'atom', atom: 'hello' },
	];
	let i = 0;
	for (const answer of query) {
		const cmp = want[i++];
		looseEqual(answer.bindings.X, cmp);
	}
})

// TODO: use deepEqual once we got the term classes properly exported
function looseEqual(got, want) {
	for (const [k, v] of Object.entries(want)) {
		assert.deepEqual(v, got[k]);
	}
}
