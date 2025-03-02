/** Prolog term. */
export type Term =
	| Atom
	| Compound<Functor, Args>
	| Variable
	| List
	| string
	| Numeric
	| Rational
	| Exception;
/** Float if within the safe integer range, otherwise a bigint. */
export type Numeric = number | bigint;
/** Prolog list term. */
export type List = Term[];
export type Functor = string;

export type Args = [Term, ...Term[]];
export type Goal = Atom | Compound<Functor, Args>;
export type PredicateIndicator = Compound<"/", [Atom, number]>;

/** Prolog atom term. */
export class Atom {
	functor: string;
	constructor(functor: string) {
		this.functor = functor;
	}
	get pi() {
		return piTerm(this.functor, 0);
	}
	toProlog() {
		return escapeAtom(this.functor);
	}
	toString() {
		return this.toProlog();
	}
}

/** Prolog compound term. */
export class Compound<Functor extends string, Arguments extends Args> {
	functor: Functor;
	args: Arguments;
	constructor(functor: Functor, args: Arguments) {
		this.functor = functor;
		this.args = args;
		if (typeof args?.length === "undefined")
			throw new Error("bad compound, not a list: " + functor);
	}
	get pi() {
		return new Compound("/", [new Atom(this.functor), this.args.length]);
	}
	toProlog() {
		if (this.args.length === 0) return escapeAtom(this.functor);

		if (
			this.args.length === 2 &&
			(this.functor === ":" || this.functor === "/")
		)
			return `${toProlog(this.args[0])}${this.functor}${toProlog(this.args[1])}`;

		return `${escapeAtom(this.functor)}(${this.args.map(toProlog).join(",")})`;
	}
	toString() {
		return this.toProlog();
	}
}

/** Creates Atom if args is empty, or a Compound otherwise. */
export function Atomic(
	functor: string,
	args: Term[],
): typeof args extends Args ? Compound<typeof functor, typeof args> : Atom {
	if (args.length === 0) return new Atom(functor);
	return new Compound(functor, args as Args);
}

/** Prolog rational term. */
export class Rational {
	numerator: Numeric;
	denominator: Numeric;
	constructor(numerator: Numeric, denominator: Numeric) {
		this.numerator = numerator;
		this.denominator = denominator;
	}
	toProlog() {
		return `${this.numerator} rdiv ${this.denominator}`;
	}
}

/** Prolog variable term. */
export class Variable {
	var: string;
	// attr?: Term[]; // TODO: not supported in Scryer yet
	constructor(name: string, attr?: Term[]) {
		if (!validVar(name)) throw new Error("invalid variable name: " + name);
		this.var = name;
		// if (attr && attr?.length > 0)
		// 	this.attr = attr;
	}
	toProlog() {
		// if (this.attr?.length) {
		// 	return this.attr.map(toProlog).join(",");
		// }
		return `${this.var}`;
	}
	toString() {
		return this.toProlog();
	}
}

/** Literal Prolog term that will be not be escaped or otherwise transformed. */
export class Literal {
	value;
	constructor(value: string) {
		this.value = value;
	}
	toProlog() {
		return this.value;
	}
}

/** Exception value thrown by Prolog.  */
export class Exception {
	/** The term ('ball') thrown by `throw/1`. */
	term: Term;
	constructor(term: Term) {
		this.term = term;
	}
	toProlog() {
		return toProlog(this.term);
	}
}

export function isAtom(x: unknown, name?: string): x is Atom {
	return (
		x instanceof Atom && (typeof name === "undefined" || x.functor === name)
	);
}

export function isCompound<F extends string>(
	x: unknown,
	name?: F,
	arity?: number,
): x is Compound<F, Args> {
	return (
		x instanceof Compound &&
		(typeof name === "undefined" || x.functor === name) &&
		(typeof arity === "undefined" || x.args.length === arity)
	);
}

export function isList(x: unknown): x is List {
	return Array.isArray(x) && x.every(isTerm);
}

export function isNumber(x: Term): x is number {
	return typeof x === "number";
}

export function isString(x: Term): x is string {
	return typeof x === "string";
}

export function isCallable(term: Term): term is Goal {
	return isAtom(term) || isCompound(term);
}

export function isVariable(term: unknown): term is Variable {
	return term instanceof Variable;
}

export function isTerm(term: unknown): term is Term {
	switch (typeof term) {
		case "number":
		case "bigint":
		case "string":
			return true;
	}
	return isAtom(term) || isCompound(term) || isList(term) || isVariable(term);
}

// TODO: this doesn't check for symbols, spaces, etc.
function validVar(name: unknown) {
	if (typeof name !== "string" || name.length === 0) return false;
	if (name[0] === "_") return true;
	if (name[0].toLowerCase() !== name[0]) return true;
	return false;
}

export function piTerm(name: string, arity: number) {
	return new Compound("/", [new Atom(name), arity]);
}

/** Converts the given term object into Prolog text. */
export function toProlog(obj: unknown): string {
	switch (typeof obj) {
		case "number":
			return obj.toString();
		case "bigint":
			return obj.toString();
		case "string":
			return escapeString(obj);
		/*
		case "boolean":
			return obj ? "{true}" : "{false}";
		case "undefined":
			return "{undefined}";
		*/
		case "object":
			break;
		default:
			throw new Error("can't convert object to Prolog term: " + obj);
	}

	if (obj === null) {
		throw new Error("can't convert null value to Prolog term");
		// return "{null}";
	}

	if ("toProlog" in obj && typeof obj.toProlog === "function")
		return obj.toProlog();

	if (obj instanceof Uint8Array)
		return escapeString(new TextDecoder().decode(obj));

	if (Array.isArray(obj)) return `[${obj.map(toProlog).join(",")}]`;

	throw new Error("scryer: can't convert object to Prolog term: " + obj);
}

// TODO: might be nice if escapeAtom could avoid the quoting when it can,
// but it is easier to just quote everything.
export function escapeAtom(atom: string) {
	return `'${atom
		.replaceAll("\\", "\\\\")
		.replaceAll(`'`, `\\'`)
		.replaceAll("\n", "\\n")
		.replaceAll("\t", "\\t")}'`;
}

export function escapeString(str: string) {
	return `"${str
		.replaceAll("\\", "\\\\")
		.replaceAll(`"`, `\\"`)
		.replaceAll("\n", "\\n")
		.replaceAll("\t", "\\t")}"`;
}

/*
export function fromJSON(json: string, options: JSONEncodingOptions = {}): Term {
	return JSON.parse(json, reviver(options));
};

export function toJSON(term: Term, indent?: string) {
	return JSON.stringify(term, function (_, v) {
		if (typeof v === "bigint")
			return { number: v.toString() };
		return v;
	}, indent)
}
*/
