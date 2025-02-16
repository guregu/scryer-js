// export * from './pkg/scryer_prolog.js'

import { default as initScryer, MachineBuilder } from './pkg/scryer_prolog.js';
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
		this.#machine = MachineBuilder.new().build();
	}
	query(goal: string): Iterable<LeafAnswer, void, void> {
		return this.#machine.runQuery(goal);
	}
}

type Term = PrologInteger | PrologRational | PrologFloat | PrologAtom | PrologString | PrologList | PrologCompound | PrologVariable;
type Bindings = Record<string, Term>;

export interface LeafAnswer {
	bindings: Bindings;
}

export interface PrologInteger {
	type: "integer";
	integer: number;
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
	exception: Term; // TODO: what type is this?
}
