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

/** Terms or objects that encode into Terms. Uint8Array becomes a string. */
export type Termlike =
	| Term
	| Float
	| Literal
	| Exception
	| Uint8Array
	| { toProlog: () => string };

/** Prolog atom term. */
export class Atom {
	value: string;
	constructor(functor: string) {
		this.value = functor;
	}
	get pi() {
		return piTerm(this.value, 0);
	}
	toProlog() {
		return escapeAtom(this.value);
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
export function Atomic(value: string, args: []): Atom;
export function Atomic(
	functor: string,
	args: Term[],
): typeof args extends Args ? Compound<typeof functor, typeof args> : Atom;
export function Atomic(functor: string, args: Term[]) {
	if (!args || args.length === 0) return new Atom(functor);
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
	toString() {
		return `${this.numerator}/${this.denominator}`;
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
		// matches toplevel representation of thrown errors as throw/1 terms
		return `throw(${toProlog(this.term)})`;
	}
	toString() {
		return this.toProlog();
	}
}

export function isAtom(x: unknown, name?: string): x is Atom {
	return x instanceof Atom && (typeof name === "undefined" || x.value === name);
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

export function isCallable(term: unknown): term is Goal {
	return isAtom(term) || isCompound(term);
}

export function isList(x: unknown): x is List {
	return Array.isArray(x) && x.every(isTerm);
}

export function isNumber(x: unknown): x is Numeric {
	return typeof x === "number" || typeof x === "bigint";
}

export function isString(x: unknown): x is string {
	return typeof x === "string";
}

export function isRational(x: unknown): x is Rational {
	return x instanceof Rational;
}

export function isVariable(term: unknown): term is Variable {
	return term instanceof Variable;
}

export function isException(err: unknown): err is Exception {
	return err instanceof Exception;
}

export function isTerm(term: unknown): term is Term {
	switch (typeof term) {
		case "number":
		case "bigint":
		case "string":
			return true;
	}
	return (
		isAtom(term) ||
		isCompound(term) ||
		isList(term) ||
		isVariable(term) ||
		isRational(term)
	);
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

const FLOAT_FORMAT = new Intl.NumberFormat("en-US", {
	useGrouping: false,
	minimumFractionDigits: 1,
	notation: "standard",
});

/** Number that will always encode to a Prolog float. */
export class Float {
	value: number;
	constructor(value: number) {
		this.value = value;
	}
	[Symbol.toPrimitive]() {
		return this.value;
	}
	toProlog() {
		if (!Number.isFinite(this.value))
			throw new Error(`can't encode ${this.value} to Prolog term`);
		return FLOAT_FORMAT.format(this.value);
	}
	toJSON() {
		return this.value;
	}
}

const NUMBER_FORMAT = new Intl.NumberFormat("en-US", {
	useGrouping: false,
	notation: "standard",
});

/** Converts the given term object into Prolog text. */
export function toProlog(obj: Termlike | Termlike[]): string {
	switch (typeof obj) {
		case "number":
			return NUMBER_FORMAT.format(obj);
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

/** Template literal function for escaping Prolog text. `${values}` will be interpreted as Prolog terms. */
export function prolog(text: TemplateStringsArray, ...values: Termlike[]) {
	let str = "";
	for (let i = 0; i < text.length; i++) {
		str += text[i];
		if (values[i]) str += toProlog(values[i]);
	}
	return str;
}

function needsEscape(atom: string) {
	// Although modern Prologs accept unquoted atoms with various unicode characters,
	// JS doesn't give us a native way to check unicode character classes.
	// This is a best-effort check for ASCII characters.

	if (atom.length === 0) {
		return true;
	}

	let code = atom.charCodeAt(0);
	// first character must be a-z
	if (!(code >= 97 && code <= 122)) {
		return true;
	}

	for (let i = 1; i < atom.length; i++) {
		code = atom.charCodeAt(i);
		if (
			(code >= 97 && code <= 122) /* a-z */ ||
			(code >= 65 && code <= 90) /* A-Z */ ||
			(code >= 48 && code <= 57) /* 0-9 */ ||
			code === 95 /* _ */
		) {
			continue;
		}
		return true;
	}

	return false;
}

export function escapeAtom(atom: string) {
	if (!needsEscape(atom)) return atom;

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
