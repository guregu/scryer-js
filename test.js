import test from "node:test";
import assert from "node:assert";

import {
	Prolog,
	Atom,
	Compound,
	Variable,
	Rational,
	Exception,
	Float,
	toProlog,
	prolog,
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

test("query", async (t) => {
	let readies = 0;
	const pl = new Prolog();
	pl.addEventListener("ready", () => {
		readies++;
	});
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
	assert.ok(query.ok);
	assert.deepEqual(readies, 1);
});

test("query failure", async (t) => {
	let readies = 0;
	const pl = new Prolog();
	pl.addEventListener("ready", () => {
		readies++;
	});
	let iter = 0;
	const query = pl.query("false.");
	assert.deepEqual(query.ok, undefined);
	for (const _ of query) {
		iter++;
	}
	assert.deepEqual(iter, 0);
	assert.deepEqual(query.ok, false);
	assert.deepEqual(readies, 1);

	await t.test("queryOnce", (t) => {
		const once = pl.queryOnce("false.");
		assert.deepEqual(once, false);
		assert.deepEqual(readies, 2);
	});
});

test("queryOnce returns control", async (t) => {
	let readies = 0;
	const pl = new Prolog();
	pl.addEventListener("ready", () => {
		readies++;
	});
	for (let i = 0; i < 10; i++) {
		const answer = pl.queryOnce("repeat, X = 1.");
		assert.deepEqual(answer.bindings.X, 1);
	}
	assert.deepEqual(readies, 10);
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
	let readies = 0;
	const pl = new Prolog();
	pl.addEventListener("ready", () => {
		readies++;
	});
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
	assert.deepEqual(readies, 1);

	await t.test("control returns to machine", (t) => {
		const second = pl.query(`X = ok.`).next();
		assert.deepEqual(second, {
			done: false,
			value: { bindings: { X: new Atom("ok") } },
		});
	});
});

test("concurrency", async (t) => {
	await t.test("Prolog.busy", (t) => {
		let readies = 0;
		const pl = new Prolog();
		pl.addEventListener("ready", () => {
			readies++;
		});
		assert.ok(!pl.busy);
		const query = pl.query(`X = 1 ; X = 2.`);
		assert.ok(pl.busy);
		query.next();
		assert.ok(pl.busy);
		query.next();
		query.next(); // we need to advance twice here, unfortuantely
		assert.ok(!pl.busy);
		assert.deepEqual(readies, 1);
	});

	await t.test("interrupt", (t) => {
		const pl = new Prolog({ concurrency: "interrupt" });
		const q1 = pl.query(`X = 1 ; X = 2.`);
		q1.next();
		const q2 = pl.query(`X = hello.`);
		assert.deepEqual(q2.next(), {
			done: false,
			value: { bindings: { X: new Atom("hello") } },
		});

		// old query should throw
		let threw;
		try {
			q1.next();
		} catch (ex) {
			threw = ex;
		}
		assert.ok(threw);
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
		assert.deepEqual(threw.toProlog(), "throw(hi)");
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
			term: new Atom("true"),
			text: "true",
			check: isAtom,
		},
		{
			term: new Atom(""),
			text: "''",
			check: isAtom,
		},
		{
			term: new Atom("OK"),
			text: "'OK'",
			check: isAtom,
		},
		{
			term: new Atom("_notvar"),
			text: "'_notvar'",
			check: isAtom,
		},
		{
			term: new Atom("hello\nworld"),
			text: "'hello\\nworld'",
			check: isAtom,
		},
		{
			term: new Compound("hello", [new Atom("world")]),
			text: "hello(world)",
			check: isCompound,
		},
		{
			term: "hi",
			text: `"hi"`,
			check: isString,
		},
		{
			term: [123, new Atom("abc")],
			text: `[123,abc]`,
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
			await t.test("prolog`X = ${" + item.term.toString() + "}`", (t) => {
				const tmpl = prolog`${new Variable("X")} = ${item.term}`;
				assert.deepEqual(tmpl, `X = ${item.text}`);
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

test("integer mode", async (t) => {
	const cases = {
		bigint: { X: 123n, Y: 1, Z: 9007199254740992n },
		fit: { X: 123, Y: 1, Z: 9007199254740992n },
		number: { X: 123, Y: 1, Z: 9007199254740992 /* Z might be inaccurate */ },
	};
	for (const [mode, want] of Object.entries(cases)) {
		await t.test(mode, async (t) => {
			const pl = new Prolog();
			const q = pl.queryOnce(
				"X = 123, Y = 1.0, Z = 9007199254740992, integer(X), float(Y), integer(Z).",
				{ integers: mode },
			);
			assert.deepEqual(q.bindings, want);
		});
	}
});

test("Float type", async (t) => {
	const f1 = new Float(123);
	assert.deepEqual(JSON.stringify(f1), "123");
	assert.deepEqual(f1.toProlog(), "123.0");

	const pl = new Prolog();
	const q1 = pl.queryOnce("true.", {
		bind: { X: new Float(1), Y: 2n },
		integers: "bigint",
	});
	assert.deepEqual(q1.bindings, { X: 1.0, Y: 2n });
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
