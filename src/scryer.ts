import { default as initScryer, MachineBuilder, QueryState } from './pkg/scryer_prolog.js';
import scryer_wasm from './pkg/scryer_prolog_bg.wasm';

const initOnce = async function init() {
	const module = await WebAssembly.compile(scryer_wasm as unknown as Uint8Array);
	await initScryer(module);
}();

export async function init() {
	await initOnce;
}

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
	query(goal: string): Query {
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
export class Query {
	#iter;
	constructor(iter: QueryState) {
		this.#iter = iter;
	}
	[Symbol.iterator]() {
		return this.#iter;
	}
	/**
	* Drops the query.
	*
	* This is useful to end a query early. Like finishing a query, control will be given back
	* to the `Machine` and any call to `next` after that will result in an error.
	*/
	drop() {
		return this.#iter.drop();
	}
}

type Term = PrologInteger | PrologRational | PrologFloat | PrologAtom | PrologString | PrologList | PrologCompound | PrologVariable;
type Bindings = Record<string, Term>;

export interface LeafAnswer {
	type: "leafAnswer";
	bindings: Bindings;
}

export interface PrologInteger {
	type: "integer";
	integer: bigint;
}

export interface PrologRational {
	type: "rational";
	numerator: bigint;
	denominator: bigint;
}

export interface PrologFloat {
	type: "float";
	float: number;
}

export interface PrologAtom {
	type: "atom";
	atom: string;
}

export interface PrologString {
	type: "string";
	string: string;
}

export interface PrologList {
	type: "list";
	list: Term[];
}

export interface PrologCompound {
	type: "compound";
	functor: string;
	args: Term[];
}

export interface PrologVariable {
	type: "variable";
	variable: string;
}

export interface PrologException {
	type: "exception";
	exception: Term;
}
