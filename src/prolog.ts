import {
	default as initScryer,
	MachineBuilder,
	QueryState,
} from "./pkg/scryer_prolog.js";
import scryer_wasm from "./pkg/scryer_prolog_bg.wasm";
import {
	Args,
	Atom,
	Compound,
	Exception,
	Rational,
	Term,
	toProlog,
	Variable,
} from "./term.js";

const initOnce = (async function init() {
	const module = await WebAssembly.compile(
		scryer_wasm as unknown as Uint8Array,
	);
	await initScryer(module);
})();

export async function init() {
	await initOnce;
}

export type QueryOptions = {
	/** Optional variable bindings to prepend to a query. */
	bind?: Bindings;
};

/** Prolog interpreter. */
export class Prolog {
	#machine;
	constructor() {
		this.#machine = new MachineBuilder().build();
	}
	/**
	 * Runs a query.
	 *
	 * You can only have one query at a time. If you try to do anything with this machine while
	 * doing a query an error will be thrown.
	 */
	query(goal: string, options: QueryOptions = {}): Query {
		if (options.bind) {
			goal = bindVars(goal, options.bind);
		}
		const iter = this.#machine.runQuery(goal);
		return new Query(iter);
	}
	/**
	 * Consults a module.
	 */
	consultText(program: string, module = "user") {
		this.#machine.consultModuleString(module, program);
	}
}

/** Running query. */
export class Query implements Iterable<Answer, void, void> {
	#iter: QueryState;
	#done = false;
	constructor(iter: any /* QueryState */) {
		// ^ want to avoid exporting QueryState
		this.#iter = iter;
	}
	[Symbol.iterator]() {
		return this;
	}
	next(): IteratorResult<Answer, void> {
		if (this.#done) {
			return { done: true, value: undefined };
		}

		const got = this.#iter.next() as IteratorResult<ScryerAnswer, void>;
		this.#done = got.done ?? false;
		if (got.done) {
			return { done: got.done, value: undefined };
		}

		if (got.value.type === "exception") {
			throw new Exception(convert(got.value.exception));
		}

		// transform from internal format to public one
		const entries = Object.entries(got.value.bindings).map(([k, v]) => [
			k,
			convert(v),
		]);
		return {
			value: {
				bindings: Object.fromEntries(entries),
			},
			done: false,
		};
	}
	/**
	 * Drops the query.
	 *
	 * This is useful to end a query early. Like finishing a query, control will be given back
	 * to the `Machine`.
	 */
	return(): IteratorReturnResult<void> {
		if (!this.#done) {
			this.#iter.drop();
			this.#done = true;
		}
		return {
			done: true,
			value: undefined,
		};
	}
}

/** Leaf answer to a Prolog query. */
export interface Answer {
	/** Variable bindings. */
	bindings: Record<string, Term>;
}

/** Variable bindings. */
export type Bindings = Record<string, Term>;

type ScryerTerm =
	| ScryerInteger
	| ScryerRational
	| ScryerFloat
	| ScryerAtom
	| ScryerString
	| ScryerList
	| ScryerCompound
	| ScryerVariable
	| ScryerException;
type ScryerBindings = Record<string, ScryerTerm>;

type ScryerAnswer =
	| {
			type: "leafAnswer";
			bindings: ScryerBindings;
	  }
	| ScryerException;

interface ScryerInteger {
	type: "integer";
	integer: bigint;
}

interface ScryerRational {
	type: "rational";
	numerator: bigint;
	denominator: bigint;
}

interface ScryerFloat {
	type: "float";
	float: number;
}

interface ScryerAtom {
	type: "atom";
	atom: string;
}

interface ScryerString {
	type: "string";
	string: string;
}

interface ScryerList {
	type: "list";
	list: ScryerTerm[];
}

interface ScryerCompound {
	type: "compound";
	functor: string;
	args: ScryerTerm[];
}

interface ScryerVariable {
	type: "variable";
	variable: string;
}

interface ScryerException {
	type: "exception";
	exception: ScryerTerm;
}

function convert(term: ScryerTerm): Term {
	if (!isScryerTerm(term)) {
		throw new Error(`not a term: ${term}`);
	}
	switch (term.type) {
		case "integer":
			return int(term.integer);
		case "rational":
			return new Rational(int(term.numerator), int(term.denominator));
		case "float":
			return term.float;
		case "atom":
			return new Atom(term.atom);
		case "string":
			return term.string;
		case "list":
			return term.list.map(convert);
		case "compound":
			const args = term.args.map(convert) as Args;
			return new Compound(term.functor, args);
		case "variable":
			return new Variable(term.variable);
		case "exception":
			return new Exception(convert(term.exception));
	}
}

function isScryerTerm(v: unknown): v is ScryerTerm {
	return (
		v !== null &&
		typeof v === "object" &&
		"type" in v &&
		typeof v.type === "string"
	);
}

function int(x: bigint): number | bigint {
	if (x > Number.MAX_SAFE_INTEGER || x < Number.MIN_SAFE_INTEGER) {
		return x;
	}
	return Number(x);
}

function bindVars(query: string, bind: Bindings) {
	const vars = Object.entries(bind)
		.map(([k, v]) => `${k} = ${toProlog(v)}`)
		.join(", ");
	if (vars.length === 0) return query;
	return `${vars}, ${query}`;
}
