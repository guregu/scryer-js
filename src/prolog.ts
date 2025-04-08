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
	isRational,
	Rational,
	Term,
	Termlike,
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
	bind?: Record<string, Termlike>;
};

export type PrologOptions = {
	/** Strategy for handling concurrent queries (which are currently unsupported).
	 * `throw` will throw an exception if you try to start a new query while one is already active.
	 * `interrupt` will drop the existing query before starting a new one.
	 *
	 * @default "throw"
	 */
	concurrency: "throw" | "interrupt";
};

/** Name of the event emitted when interpreter is ready to query. */
export const EVENT_READY = "ready";

/**
 * Prolog interpreter.
 *
 * @fires Event#ready
 */
export class Prolog extends EventTarget {
	#machine;
	#running?: Query;
	#shouldInterrupt: boolean;
	constructor(options?: Partial<PrologOptions>) {
		super();
		this.#machine = new MachineBuilder().build();
		this.#shouldInterrupt = options?.concurrency === "interrupt";
	}
	/**
	 * Runs a query.
	 *
	 * You can only have one query at a time. If you try to do anything with this machine while
	 * doing a query an error will be thrown.
	 */
	query(goal: string, options: QueryOptions = {}): Query {
		if (this.#shouldInterrupt && this.busy) {
			this.#running?.return(true);
		}

		if (options.bind) {
			goal = bindVars(goal, options.bind);
		}
		const iter = this.#machine.runQuery(goal);
		const query = new Query(this, iter);
		this.#running = query;
		return query;
	}
	/** Runs a query, returning at most one answer and discarding any others. */
	queryOnce(goal: string, options: QueryOptions = {}): Answer | false {
		let query;
		try {
			query = this.query(goal, options);
			const answer = query.next();
			if (answer.done) return false;
			return answer.value;
		} finally {
			query?.return();
		}
	}
	/**
	 * Consults a module.
	 */
	consultText(program: string, module = "user") {
		this.#machine.consultModuleString(module, program);
	}

	/** Returns true if there is a currently running query. */
	get busy() {
		return this.#running ? !this.#running.done : false;
	}
}

/** Running query. An iterator that yields `Answer` on success and returns false on failure. */
export class Query implements Iterable<Answer, boolean, void> {
	#pl: Prolog;
	#iter: QueryState;
	#done = false;
	#ok = 0;
	#interrupted = false;
	constructor(pl: Prolog, iter: any /* QueryState */) {
		// ^ want to avoid exporting QueryState
		this.#pl = pl;
		this.#iter = iter;
	}
	[Symbol.iterator]() {
		return this;
	}
	next(): IteratorResult<Answer, boolean> {
		if (this.#interrupted) {
			throw new Error("query interrupted by newer query");
		}
		if (this.#done) {
			return this.#coda();
		}

		const got = this.#iter.next() as IteratorResult<ScryerResult, void>;
		this.#done = got.done ?? false;
		if (got.done) {
			this.#dispatchReady();
			return this.#coda();
		}

		// query failed
		if (got.value === false) {
			this.#done = true;
			// TODO: not 100% sure why this drop is necessary, looks like QueryState needs to iterate once past a failure
			// otherwise the Machine holds on to it
			this.#iter.drop();
			this.#dispatchReady();
			return this.#coda();
		}

		// query threw
		if (got.value.type === "exception") {
			this.#done = true;
			this.#iter.drop();
			this.#dispatchReady();
			throw new Exception(convert(got.value.exception));
		}

		// query success
		this.#ok++;
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
	return(interrupted?: boolean): IteratorReturnResult<boolean> {
		if (!this.#done) {
			this.#iter.drop();
			this.#done = true;
			this.#interrupted = !!interrupted;
			if (!this.#interrupted) {
				this.#dispatchReady();
			}
		}
		return this.#coda();
	}

	/** True if this query has succeeded at least once, false if it failed, undefined if it hasn't run yet. */
	get ok(): boolean | undefined {
		if (!this.#done && this.#ok === 0) return undefined;
		return this.#ok > 0;
	}

	get done(): boolean {
		return this.#done;
	}

	#coda(): IteratorReturnResult<boolean> {
		return { done: true, value: this.#ok > 0 };
	}

	#dispatchReady() {
		this.#pl.dispatchEvent(new Event(EVENT_READY));
	}
}

/** Leaf answer to a Prolog query. */
export interface Answer {
	/** Variable bindings. */
	bindings: Bindings;
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

type ScryerResult =
	| ScryerAnswer // query succeeded
	| false // query failed
	| ScryerException; // query threw

interface ScryerAnswer {
	type: "leafAnswer";
	bindings: ScryerBindings;
}

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

function bindVars(query: string, bind: Record<string, Termlike>) {
	const vars = Object.entries(bind)
		.map(([k, v]) => `${k} ${isRational(v) ? "is" : "="} ${toProlog(v)}`)
		.join(", ");
	if (vars.length === 0) return query;
	return `${vars}, ${query}`;
}
