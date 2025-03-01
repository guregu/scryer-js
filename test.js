import test from "node:test";
import assert from "node:assert";

import { init, Prolog, Atom, Rational, Compound, Exception } from "./dist/scryer.js";

await test("load", async (t) => {
	await init();
});

test("query", async (t) => {
	const pl = new Prolog();
	const query = pl.query(`
		  X = 1
		; X = 9007199254740991  % max safe integer
		; X = 9007199254740992
		; X = -9007199254740991 % min safe integer
		; X = -9007199254740992
		; X is 1 rdiv 3
		; X = hello
		; X = hello(world)
		; X = [1, 2, 3]
		; X = "hello".
	`);
	const want = [
		1,
		9007199254740991,
		9007199254740992n,
		-9007199254740991,
		-9007199254740992n,
		new Rational(1, 3),
		new Atom("hello"),
		new Compound("hello", [new Atom("world")]),
		[1, 2, 3],
		"hello",
	];
	let i = 0;
	for (const answer of query) {
		const cmp = want[i++];
		assert.deepEqual(answer.bindings.X, cmp);
	}
});

test("query var binding", async (t) => {
	const pl = new Prolog();
	const query = pl.query(`X = hello(Planet).`, { bind: { Planet: new Atom("world") } });
	const want = [
		new Compound("hello", [new Atom("world")])
	];
	let i = 0;
	for (const answer of query) {
		const cmp = want[i++];
		assert.deepEqual(answer.bindings.X, cmp);
	}
});

test("query drop early", async (t) => {
	const pl = new Prolog();
	const query = pl.query(`repeat, X = 1.`);
	const want = [
		1,
		1,
		1,
	];
	let i = 0;
	for (const answer of query) {
		const cmp = want[i++];
		assert.deepEqual(answer.bindings.X, cmp);

		if (i === 3) break;
	}
	const extra = query.next();
	assert.deepEqual(extra, { done: true, value: undefined });

	t.test("control returns to machine", (t) => {
		const second = pl.query(`X = ok.`).next();
		assert.deepEqual(second, { done: false, value: { bindings: { X: new Atom("ok") } } });
	});
});

test("throw/1", async (t) => {
	const pl = new Prolog();
	let threw;
	try {
		const query = pl.query("throw(hi).");
		query.next();
	} catch (ex) {
		threw = ex;
	} finally {
		if (!threw) {
			assert.fail("didn't throw");
		}
		if (!(threw instanceof Exception)) {
			assert.fail(`bad exception: ${ex}`);
		}
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
		new Atom("world"),
		new Atom("Welt"),
		new Atom("世界"),
		new Atom("yes"),
	];
	let i = 0;
	try {
		for (const answer of query) {
			const cmp = want[i++];
			assert.deepEqual(answer.bindings.X, cmp);
		}
	} catch (ex) {
		console.log(ex.cause);
		throw ex;
	}
});
