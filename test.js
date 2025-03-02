import test from "node:test";
import assert from "node:assert";

import {
	init,
	Prolog,
	Atom,
	Compound,
	Variable,
	Rational,
	Exception,
	toProlog,
	isTerm,
	isNumber,
	isString,
	isAtom,
	isCompound,
	isList,
	isVariable,
	isRational,
	isException,
} from "./dist/scryer.js";

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
	assert.deepEqual(i, want.length);
});

test("query failure", async (t) => {
	const pl = new Prolog();
	let iter = 0;
	for (const _ of pl.query("false.")) {
		iter++;
	}
	assert.deepEqual(iter, 0);

	await t.test("queryOnce", (t) => {
		const once = pl.queryOnce("false.");
		assert.deepEqual(once, false);
	});
});

test("queryOnce returns control", async (t) => {
	const pl = new Prolog();
	for (let i = 0; i < 10; i++) {
		const answer = pl.queryOnce("repeat, X = 1.");
		assert.deepEqual(answer.bindings.X, 1);
	}
});

test("query var binding", async (t) => {
	const pl = new Prolog();
	const query = pl.query(`X = hello(Planet).`, {
		bind: { Planet: new Atom("world") },
	});
	const want = [new Compound("hello", [new Atom("world")])];
	let i = 0;
	for (const answer of query) {
		const cmp = want[i++];
		assert.deepEqual(answer.bindings.X, cmp);
	}
	assert.deepEqual(i, want.length);
});

test("query drop early", async (t) => {
	const pl = new Prolog();
	const query = pl.query(`repeat, X = 1.`);
	const want = [1, 1, 1];
	let i = 0;
	for (const answer of query) {
		const cmp = want[i++];
		assert.deepEqual(answer.bindings.X, cmp);

		if (i === 3) break;
	}
	const extra = query.next();
	assert.deepEqual(extra, { done: true, value: true });

	t.test("control returns to machine", (t) => {
		const second = pl.query(`X = ok.`).next();
		assert.deepEqual(second, {
			done: false,
			value: { bindings: { X: new Atom("ok") } },
		});
	});
});

test("throw/1", async (t) => {
	const pl = new Prolog();
	let threw;
	let query;
	try {
		query = pl.queryOnce("throw(hi).");
		query.next();
	} catch (ex) {
		threw = ex;
	} finally {
		query?.return();
		if (!threw) {
			assert.fail("didn't throw");
		}
		if (!(threw instanceof Exception)) {
			assert.fail(`bad exception: ${ex}`);
		}
		assert.deepEqual(threw.term, new Atom("hi"));
		assert.ok(isException(threw));
		assert.deepEqual(threw.toProlog(), "throw('hi')");
	}

	await t.test("returns control", async (t) => {
		for (let i = 0; i < 10; i++) {
			try {
				pl.queryOnce(`throw(${i}).`);
			} catch (ex) {
				if (!isException(ex)) assert.fail(`bad exception: ${ex} #${i}`);
				assert.deepEqual(ex.term, i);
			}
		}
		await t.test("success after throw returns", async (t) => {
			t.skip("BUG: throw ball is sticky?");
			return;
			const extra = pl.queryOnce("X = 123.");
			assert.deepEqual(extra.bindings.X, 123);
		});
	});
});

test("terms", async (t) => {
	const cases = [
		{
			term: 1,
			text: "1",
			check: isNumber,
		},
		{
			term: 9007199254740992n,
			text: "9007199254740992",
			check: isNumber,
		},
		{
			term: new Rational(1, 3),
			text: "1 rdiv 3",
			check: isRational,
		},
		{
			term: new Atom("y'all"),
			text: "'y\\'all'",
			check: isAtom,
		},
		{
			term: new Atom("OK"),
			text: "'OK'",
			check: isAtom,
		},
		{
			term: new Compound("hello", [new Atom("world")]),
			text: "'hello'('world')",
			check: isCompound,
		},
		{
			term: "hi",
			text: `"hi"`,
			check: isString,
		},
		{
			term: [123, new Atom("abc")],
			text: `[123,'abc']`,
			check: isList,
		},
		{
			term: new Variable("X"),
			text: "X",
			check: isVariable,
		},
	];
	await t.test(`term to prolog text`, async (t) => {
		for (const item of cases) {
			await t.test(`toProlog(${item.term})`, (t) => {
				assert.deepEqual(toProlog(item.term), item.text);
			});
		}
	});
	await t.test(`term type checkers`, async (t) => {
		for (const item of cases) {
			await t.test(`${item.check.name}(${item.term})`, (t) => {
				assert.ok(item.check(item.term));
			});
			await t.test(`isTerm(${item.term})`, (t) => {
				assert.ok(isTerm(item.term));
			});
		}
	});
	// make sure we get the same term type back that we bind with QueryOptions
	await t.test(`binding roundtrip`, async (t) => {
		const pl = new Prolog();
		for (const item of cases) {
			await test(`X = ${toProlog(item.term)}`, (t) => {
				if (isVariable(item.term)) {
					t.skip("equivalent variables don't get individual bindings (yet?)");
					return;
				}
				const ans = pl.queryOnce("X = Y.", { bind: { Y: item.term } });
				assert.deepEqual(ans.bindings.X, item.term);
			});
		}
	});
});

test("consult module", async (t) => {
	const pl = new Prolog();
	pl.consultText(
		`
		:- module(test_module, [working/1]).
		working(yes).
	`,
		"test_module",
	);
	// TODO: use_module/1 not importing things properly?
	pl.consultText(
		`
		:- use_module(test_module).
		hello(world).
		hello('Welt').
		hello(世界).
		hello(X) :- test_module:working(X).
	`,
		"user",
	);
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
	assert.deepEqual(i, want.length);
});
