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
		assert.deepEqual(answer.bindings.X, cmp);
	}
});

test("consult module", async (t) => {
	const pl = new Prolog();
	pl.consultText(`
		:- module(test_module, [working/1]).
		working(yes).
	`, "test_module");
	// TODO: use_module/1 not importing things properly?
	pl.consultText(`
		:- use_module(test_module).
		hello(world).
		hello('Welt').
		hello(世界).
		hello(X) :- test_module:working(X).
	`, "user");
	const query = pl.query("hello(X).");
	const want = [
		{ type: 'atom', atom: 'world' },
		{ type: 'atom', atom: 'Welt' },
		{ type: 'atom', atom: '世界' },
		{ type: 'atom', atom: 'yes' },
	];
	let i = 0;
	for (const answer of query) {
		const cmp = want[i++];
		assert.deepEqual(answer.bindings.X, cmp);
	}
});
