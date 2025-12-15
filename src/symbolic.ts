/* eslint-disable no-restricted-syntax */
import { Operators, compare, isInstance} from '@isopodlabs/maths/core';
import real from '@isopodlabs/maths/real';
import integer from '@isopodlabs/maths/integer';
import rational from '@isopodlabs/maths/rational';
import { toSuperscript, radicalChars } from '@isopodlabs/maths/string';

// invariants:
// - all symbolic instances are interned and unique by id
// - symbolic instances are immutable
// - symbolic instances with same id are structurally equal
//	- Constants
//		- integers >= 0 in symbolicConst
//		- rationals and integers < 0 in symbolicMul(i, [])
//		- radicals in
//	- Additive:
// 		- terms are sorted and combined - no duplicate terms
//		- terms are not constants (constant term is stored separately)
//		- each term has an integer multiplier, and the gcd of all multipliers must be 1
//		- multiplicative terms store their constant in the term *not* in the item
//
//	- Multiplicative:
// 		- factors are sorted and combined - no duplicate factors
//		- factors are not constants (constant factor is stored separately)
//		- each factor has a rational power
//		- constant factor is rational

class Interner<T extends object> {
	private table = new Map<string, WeakRef<T>>();
	private finalizer: FinalizationRegistry<string>;

	constructor() {
		this.finalizer = new FinalizationRegistry((key: string) => {
			const ref = this.table.get(key);
			if (ref && !ref.deref())
				this.table.delete(key);
		});
	}

	intern<U extends T>(key: string, factory: (key: string) => U): U {
		return (this.get(key) || this.set(key, factory(key))) as U;
	}

	get(key: string): T | undefined {
		return this.table.get(key)?.deref?.();
	}

	set(key: string, value: T): T {
		this.table.set(key, new WeakRef(value));
		this.finalizer.register(value, key);
		return value;
	}

	prune(): void {
		for (const [k, ref] of this.table) {
			if (ref.deref?.() === undefined)
				this.table.delete(k);
		}
	}
}

export type Bindings = Record<string, symbolic>;

export interface Visitor {
	noRemake?:boolean;
	pre?:	(node: symbolic, parent?: symbolic) => symbolic | undefined;
	post?:	(node: symbolic, parent?: symbolic) => symbolic | undefined;
}

function visit(node: symbolicBase, recurse: () => symbolicBase, visitor: Visitor, parent?: symbolicBase, children?: () =>symbolicBase[]): symbolicBase {
	function postvisit(node: symbolicBase): symbolicBase {
		return visitor.post?.(node as symbolic, parent as symbolic) ?? node;
	}
	if (visitor.pre) {
		const n = visitor.pre(node as symbolic, parent as symbolic);
		if (!n)
			return node;
		if (n !== node)
			return postvisit(n);
	}
	if (visitor.noRemake)
		children?.().forEach(child => child.visit(visitor, node as symbolic));
	else
		node = recurse();
	return postvisit(node);
}

export type MatchOptions = {
	exact?:		boolean;
	depth?:		number;
	scaleTest?:	'exact'|'multiple'|'any';
	powerTest?:	'exact'|'multiple'|'any';
};

export type ExpandOptions = {
	depth?:		number;
	maxPow?:	number;
	uses?:		Set<string>;	// only expand if these variables are present
	maxParts?:	number;			// cap number of expansion parts to avoid combinatorial explosion
};

const StringifyOptionsDefault = {
	parentheses:	'minimal' as 'minimal' | 'always' | 'never',
	direct:			false,
	ccode:			false,
	superPower: 	false,
	radicalPower:	false,
	sortTerms:		true,
	sortFactors:	true,
	mulChar:		' * ',
	divChar:		' / ',
	addChar:		' + ',
	subChar:		' - ',
	printConst:		(n: rational) => n.toString(),
};

export type StringifyOptions = typeof StringifyOptionsDefault;

function printScaled(s: string, num: rational, opts: StringifyOptions) {
	if (!s)
		return opts.printConst(num);
	const sign = num.sign() < 0 ? '-' : '';
	const abs = num.abs();
	if (abs.is1())
		return sign + s;

	return `${sign}${opts.printConst(abs)}${maybeParentheses(s, opts)}`;
}

function maybeParentheses(s: string, opts: StringifyOptions) {
	if (opts.parentheses === 'always' || (opts.parentheses === 'minimal' && s.includes(' ')))
		return `(${s})`;
	return s;
}

//-----------------------------------------------------------------------------
// symbolicBase
//-----------------------------------------------------------------------------


// Match logging - RAII-style auto-indent/outdent

export class symbolicBase {
	static interner		= new Interner<symbolicBase>();
	static defStringify = StringifyOptionsDefault;
	static matchDepth	= 0;
	static matchLogging	= false;

	static set(key: symbolic, value: symbolic) {
		this.interner.set(key.id, value);
	}
	static getById(key: string) {
		return this.interner.get(key);
	}
	static setDefaultStringifyOptions(opts: Partial<StringifyOptions>) {
		this.defStringify = { ...this.defStringify, ...opts };
	}

	static setMatchLogging(enabled: boolean) {
		this.matchLogging = enabled;
	}
	static matchLog(log: ()=>string) {
		if (this.matchLogging) {
			console.log('  '.repeat(this.matchDepth++) + log());
			return () => { this.matchDepth--; };
		}
		return () => {};
	}

	constructor(public id: string) {}
	is<T extends keyof typeof types>(type: T): this is InstanceType<(typeof types)[T]> {
		return this instanceof types[type];
	}

	dup():		symbolicBase		{ return this; }
	eq(b: symbolicBase) : boolean	{ return this === b;}

	substitute(_map: Bindings):			symbolicBase	{ return this; }
	visit(visitor: Visitor, parent?: symbolicBase):		symbolicBase	{ return visit(this, () => this, visitor, parent); }

	match(_node: symbolicBase, _bindings: Bindings, _opts?: MatchOptions): Bindings | null {
		return null;
	}

	derivative(_v: string):						symbolic		{ return zero;}
	integral(_v: string):						symbolic|undefined	{ return; }
	_toString(_opts: StringifyOptions):			string			{ return this.id; }
	toString(opts?: Partial<StringifyOptions>):	string			{ return this._toString({ ...symbolic.defStringify, ...opts }); }
	[Symbol.for("debug.description")]():		string			{ return this.toString({}); }
}

//-----------------------------------------------------------------------------
// symbolic
//-----------------------------------------------------------------------------

type param = number | rational | symbolic;
type param2 = param | (() => param);

function makeRat(n: number|rational) {
	if (!real.is(n))
		return n;

	const d = real.denominator(n, 1e6, 1e-8);
	if (d < 1e6)
		return rational(Math.round(n * d), d);

	for (let i = 2; i < 4; i++) {
		const t = n ** i;
		const d = real.denominator(t, 1e6, 1e-8);
		if (d < 1e6)
			return symbolic.from(rational(Math.round(n * d), d)).rpow(1, i);
	}
	throw new Error(`Cannot convert ${n} to rational`);
}

function asNumeric(b: param): rational | symbolic {
	if (!(b instanceof symbolic))
		return makeRat(b);
	
	if (isConst(b))
		return rational(b.value);

	if (b instanceof symbolicMul && b.factors.length === 0)
		return b.num;

	return b;
}

function asSymbolic(i: param): symbolic {
	return i instanceof symbolic ? i : symbolic.from(i);
}
function asSymbolic2(i: param2): symbolic {
	return asSymbolic(typeof i === 'function' ? i() : i);
}

// LIATE rule priority for integration by parts
// Lower = better choice for u (differentiate); Higher = better choice for dv (integrate)
function liatePriority(expr: symbolic): number {
	if (expr.is('var') || expr.is('mul') || expr.is('add'))
		return 2;	// Algebraic

	if (expr.is('unary')) {
		switch (expr.name) {
			case 'sin':		case 'cos':		case 'tan':
			case 'sinh':	case 'cosh':	case 'tanh':
				return 3;				// Trig
			case 'asin':	case 'acos':	case 'atan':
			case 'asinh':	case 'acosh':	case 'atanh':
				return 1;				// Inverse trig
			case 'exp':		return 4;	// Exponential
			case 'log':		return 0;	// Logarithmic
		}
	}
	if (expr.is('binary')) {
		switch (expr.name) {
			//case 'mod':	return 0;
			//case 'min':	return 0;
			//case 'max':	return 0;
			case 'pow':		return 4;	// Exponential
			case 'atan2':	return 1;	// inverse trig
		}
	}

	return 5;	// Unknown - lowest priority for u
}

export class symbolic extends symbolicBase {
	static from(i: number|rational): symbolic {
		
		if (isInstance(i, rational))
			return i.den !== 1 || i.num < 0 ? symbolicMul.create([], i) : symbolicConstant.create(i.num);

		if (Number.isInteger(i))
			return i < 0 ? symbolicMul.create([], rational(i)) : symbolicConstant.create(i);

		if (Number.isFinite(i)) {
			const r = makeRat(i);
			if (r instanceof symbolic)
				return r;
			return this.from(r);
		}
		return	i === Infinity ? infinity
			:	i === -Infinity ? infinity.neg()
			:	nan;
	}
	static variable(name: string): symbolic	{
		const v = (symbolic as unknown as Record<string, symbolic>)[name];
		if (v instanceof symbolic)
			return v;
		return symbolicVariable.create(name);
	}
	static sym(name: string): symbolic {
		return symbolicVariable.create(name);
	}
	static bind(name: string): symbolic 	{ return symbolicMatcher.create(name); }

	static get zero()		: symbolic		{ return zero; }
	static get one()		: symbolic		{ return one; }
	static get i()			: symbolic		{ return i; }
	static get e()			: symbolic		{ return e; }
	static get pi()			: symbolic		{ return pi; }
	static get infinity()	: symbolic		{ return infinity; }
	static get nan()		: symbolic		{ return nan; }

	static sign(i: param)	: symbolic		{ return symbolicSign.create(asSymbolic(i)); }
	static frac(i: param)	: symbolic		{ return symbolicFrac.create(asSymbolic(i)); }
	static floor(i: param)	: symbolic		{ return symbolicFloor.create(asSymbolic(i)); }
	static ceil(i: param)	: symbolic		{ return symbolicCeil.create(asSymbolic(i)); }
	static trunc(i: param)	: symbolic		{ return symbolicTrunc.create(asSymbolic(i)); }
	static round(i: param)	: symbolic		{ return symbolicRound.create(asSymbolic(i)); }
	static min(a: param, b: param)	: symbolic		{ return symbolicMin.create(asSymbolic(a), asSymbolic(b)); }
	static max(a: param, b: param)	: symbolic		{ return symbolicMax.create(asSymbolic(a), asSymbolic(b)); }

	static sin(i: param)	: symbolic		{ return symbolicSin.create(asSymbolic(i)); }
	static cos(i: param)	: symbolic		{ return symbolicCos.create(asSymbolic(i)); }
	static tan(i: param)	: symbolic		{ return symbolicTan.create(asSymbolic(i)); }
	static asin(i: param)	: symbolic		{ return symbolicAsin.create(asSymbolic(i)); }
	static acos(i: param)	: symbolic		{ return symbolicAcos.create(asSymbolic(i)); }
	static atan(i: param)	: symbolic		{ return symbolicAtan.create(asSymbolic(i)); }
	static atan2(a: param, b: param): symbolic		{ return symbolicAtan2.create(asSymbolic(a), asSymbolic(b)); }

	static exp(i: param)	: symbolic		{ return symbolicExp.create(asSymbolic(i)); }
	static log(i: param)	: symbolic		{ return symbolicLog.create(asSymbolic(i)); }
	static sqrt(i: param)	: symbolic		{ return asSymbolic(i).rpow(1, 2); }

	static sinh(i: param)	: symbolic		{ return symbolicSinh.create(asSymbolic(i)); }
	static cosh(i: param)	: symbolic		{ return symbolicCosh.create(asSymbolic(i)); }
	static tanh(i: param)	: symbolic		{ return symbolicTanh.create(asSymbolic(i)); }
	static asinh(i: param)	: symbolic		{ return symbolicAsinh.create(asSymbolic(i)); }
	static acosh(i: param)	: symbolic		{ return symbolicAcosh.create(asSymbolic(i)); }
	static atanh(i: param)	: symbolic		{ return symbolicAtanh.create(asSymbolic(i)); }

	static re(i: param)		: symbolic		{
		if (!(i instanceof symbolic))
			return symbolic.from(i);
		if (i === symbolic.i)
			return zero;
		if (i.is('add'))
			return addTerms(i.num, ...i.terms.map((i) => term(this.re(i.item), i.coef)));
		if (i.is('mul')) {
			for (const f of i.factors)
				if (f.pow.is1() && f.item === this.i)
					return zero;
		}
		return symbolicRe.create(i);
	}

	static im(i: param)		: symbolic		{
		if (!(i instanceof symbolic))
			return zero;
		if (i === symbolic.i)
			return one;
		if (i.is('add'))
			return addTerms(0 as integer, ...i.terms.map((i) => term(this.im(i.item), i.coef)));
		if (i.is('mul')) {
			const factors = i.factors.filter(f => !f.pow.is1() || f.item !== this.i);
			if (factors.length === i.factors.length)
				return zero;
			return mulFactors(i.num, ...factors);
		}
		return symbolicIm.create(i);
	}

	static conj(i: param)	: symbolic		{ return i instanceof symbolic ? this.re(i).sub(this.im(i).mul(this.i)) : symbolic.from(i); }
	static arg(i: param)	: symbolic		{ return i instanceof symbolic ? symbolicAtan2.create(this.im(i), this.re(i)) : zero; }

	static switch(otherwise: param2, ...args: [symbolicBoolean, param2][]): symbolic {
		return combinePartitions(asSymbolic2(otherwise), ...args.map(([bool, value]) => partition(bool, asSymbolic2(value))));
	}

	from(n: number) 		{ return symbolic.from(n); }
	dup():		symbolic	{ return this; }

	compare(op: OpNames, b: param): symbolicBoolean {
		return symbolicCompare.create(Ops[op], this, asSymbolic(b));
	}
	less(b: param): symbolicBoolean {
		return symbolicCompare.create(OpMasks.LT, this, asSymbolic(b));
	}
	greater(b: param): symbolicBoolean {
		return symbolicCompare.create(OpMasks.GT, this, asSymbolic(b));
	}

	scale(b: number|rational): symbolic	{
		const s = makeRat(b);
		return s instanceof symbolic ? this.mul(s) : s.is0() ? zero : s.is1() ? this : mulFactors(s, factor(this));
	}
	npow(b: number|rational): symbolic {
		const s = makeRat(b);
		return s instanceof symbolic ? this.pow(s) : s.is1() ? this : s.is0() ? one : mulFactors(rational(1), factor(this, s));
	}
	ipow(b: number): symbolic				{ return this.npow(b); }
	rpow(n: number, d: number): symbolic	{ return this.npow(rational(n, d)); }

	sqrt():		symbolic	{ return this.rpow(1, 2); }
	abs():		symbolic	{ return symbolicAbs.create(this); }
	neg():		symbolic	{ return symbolicMul.create([factor(this)], rational(-1)); }
	recip():	symbolic	{ return this.npow(-1); }
	conj():		symbolic	{ return symbolic.conj(this); }

	pow(b: param): symbolic {
		b = asNumeric(b);
		if (b instanceof symbolic)
			return symbolicPow.create(this, b);
		else
			return this.npow(b);
	}

	add(b: param): symbolic	{
		b = asNumeric(b);
		return	!(b instanceof symbolic) ? (b.is0() ? this : addTerms(b, term(this)))
			:	b instanceof symbolicAdd ? addTerms(b.num, term(this), ...b.terms)
			:	addTerms(0 as integer, term(this), term(b));
	}
	sub(b: param): symbolic	{
		b = asNumeric(b);
		return	!(b instanceof symbolic) ? (b.is0() ? this : addTerms(b.neg(), term(this)))
			:	b instanceof symbolicAdd ? addTerms(-b.num as integer, term(this), ...b.terms.map(i => term(i.item, -i.coef)))
			:	addTerms(0 as integer, term(this), term(b, -1));
	}
	mul(b: param): symbolic	{
		b = asNumeric(b);
		return !(b instanceof symbolic) ? this.scale(b)
			: b instanceof symbolicMul ? mulFactors(b.num, ...b.factors, factor(this))
			: mulFactors(rational(1), factor(this), factor(b));
	}

	div(b: param): symbolic	{
		b = asNumeric(b);
		return !(b instanceof symbolic) ? (b.is0() ? infinity : this.scale(b.recip()))
			: b instanceof symbolicMul ? mulFactors(b.num.recip(), ...b.factors.map(i => factor(i.item, i.pow.neg())), factor(this))
			: mulFactors(rational(1), factor(this), factor((b as symbolic).recip()));
	}
	mod(b: param): symbolic		{ return symbolicMod.create(this, asSymbolic(b)); }

	derivative(_v: string):			symbolic { return zero;}
	integral(_v: string):			symbolic { return zero; }
	substitute(map: Bindings):		symbolic { return super.substitute(map) as symbolic; }
	expand(_opts?: ExpandOptions):	symbolic { return this; }
	collect(v: string|symbolic):	symbolic[] {
		const r: symbolic[] = [];
		r[v === this ? 1 : 0] = this;
		return r;
	}
	integrate(v: string, from: param, to: param):	symbolic {
		const int = this.integral(v);
		return int.substitute({[v]: asSymbolic(to)}).sub(int.substitute({[v]: asSymbolic(from)}));
	}

	visit(visitor: Visitor, parent?: symbolicBase): symbolic { return super.visit(visitor, parent) as symbolic; }

	// non-symbolic ops
	
	mag(): number				{ const v = this.evaluate(); return Number.isFinite(v) ? Math.abs(v) : NaN; }
	eq(b: symbolic): boolean	{ return this === b;}
	sign(): number				{ return Math.sign(this.evaluate()); }
	lt(b: symbolic): boolean	{ return this.evaluate() < b.evaluate(); }

	evaluate(_env?: Record<string, number>):	number			{ return NaN; }
	evaluateT<T>(_ops: Operators<T>, _env?: Record<string, T>) 	:	T|undefined	{ return undefined; }
	valueOf():									number 			{ return this.evaluate(); }

	// Operators<symbolic>

	static func(name: string, args: symbolic[]): symbolic | undefined {
		const staticFn = (this as any)[name];
		if (typeof staticFn === 'function')
			return staticFn.apply(this, args as any);
		const protoFn = (this.prototype as any)[name];
		if (typeof protoFn === 'function')
			return protoFn.apply(args[0], (args as any).slice(1));

		return undefined;
	}
	static dup(a: symbolic): symbolic						{ return a; }
	static neg(a: symbolic): symbolic						{ return a.mul(-1); }
	static scale(a: symbolic, b: number): symbolic			{ return a.mul(b); }
	static add(a: symbolic, b: symbolic): symbolic			{ return a.add(b); }
	static sub(a: symbolic, b: symbolic): symbolic			{ return a.sub(b); }
	static mul(a: symbolic, b: symbolic): symbolic			{ return a.mul(b); }
	static div(a: symbolic, b: symbolic): symbolic			{ return a.div(b); }
	static ipow(a: symbolic, b: number): symbolic			{ return a.ipow(b); }
	static rpow(a: symbolic, n: number, d: number): symbolic{ return a.rpow(n, d); }
	static pow(a: param, b: param): symbolic				{ return asSymbolic(a).pow(b); }
	static eq(a: symbolic, b: symbolic): symbolicBoolean	{ return a.compare('=', b); }
	static lt(a: symbolic, b: symbolic): symbolicBoolean	{ return a.compare('<', b); }
}

//-----------------------------------------------------------------------------
// constant
// only integers >= 0
//-----------------------------------------------------------------------------

class symbolicConstant extends symbolic {
	static validate(i: number)  {
		if (!Number.isInteger(i) || i < 0)
			throw new Error("Invalid constant");
	}

	static create(i: number) {
		this.validate(i);
		return this.interner.intern(`c:${i}`, id => new symbolicConstant(id, i));
	}

	constructor(id: string, public value: number) {
		super(id);
	}

	abs():		symbolic	{ return this; }
	neg():		symbolic	{ return symbolicMul.create([], rational(-this.value)); }

	integral(v: string) {
		return this.mul(symbolicVariable.create(v));
	}

	evaluate(_env?: Record<string, number>) 					{ return this.value; }
	evaluateT<T>(ops: Operators<T>, _env?: Record<string, T>)	{ return ops.from(this.value); }
	_toString(opts: StringifyOptions) 							{ return opts.printConst(rational(this.value)); }
}

const zero	= symbolic.from(0);
const one	= symbolic.from(1);

function isConst(e: symbolic): e is symbolicConstant {
	return e instanceof symbolicConstant;
}

//-----------------------------------------------------------------------------
// variable
//-----------------------------------------------------------------------------

class symbolicVariable extends symbolic {
	static create(name: string)	{ return this.interner.intern(`v:${name}`, id => new symbolicVariable(id, name)); }

	constructor(id: string, public name: string) {
		super(id);
	}
	substitute(map: Bindings) : symbolic {
		const val = map[this.name];
		return val === undefined ? this : val;
	}
	derivative(v: string) : symbolic {
		return v === this.name ? one : zero;
	}
	integral(v: string) : symbolic {
		return v === this.name ? this.npow(2).div(2) : this.mul(symbolicVariable.create(v));
	}
	evaluate(env?: Record<string, number>) : number {
		return env?.[this.name] ?? NaN;
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return env ? env[this.name] : undefined;
	}
	_toString() : string { return this.name; }
}

function varName(v: symbolic, defaultName = 'x') {
	return v.is('var') ? v.name : defaultName;
}

class derivMatcher extends symbolic {
	constructor(public name: string, public v: string) {
		super('?');
	}
	match(node: symbolic, bindings: Bindings, _opts?: MatchOptions): Bindings | null {
		const got	= bindings[this.name];
		const v		= varName(bindings[this.v]);
			
		if (got) {
			if (!node.eq(got.derivative(v)))
				return null;
			return bindings;
		}

		const dgot = bindings['d'+this.name];
		if (dgot) {
			if (dgot !== node)
				return null;
			return bindings;
		}

		bindings['d'+this.name] = node;
		symbolic.matchLog(()=> `bound d${this.name} to ${node}`)();
		return bindings;
	}
	_toString() : string { return 'd' + this.name; }
};

export class symbolicMatcher extends symbolicVariable {
	static create(name: string)	{ return this.interner.intern(`*:${name}`, id => new symbolicMatcher(id, name)); }

	match(node: symbolic, bindings: Bindings, _opts?: MatchOptions): Bindings | null {
		const got = bindings[this.name];
		if (got) {
			if (!got.eq(node))
				return null;
			return bindings;
		}

		const dgot = bindings['d'+this.name];
		if (dgot) {
			if (!dgot.eq(node.derivative(varName(bindings['wrt']))))
				return null;
		}

		bindings[this.name] = node;
		symbolic.matchLog(()=> `bound ${this.name} to ${node}`)();
		return bindings;
	}
	derivative(v: string) : symbolic {
		return new derivMatcher(this.name, v);
	}
}

//-----------------------------------------------------------------------------
// add
//-----------------------------------------------------------------------------

export interface term {
	item:	symbolic;
	coef:	integer;
}
export function term(item: symbolic, coef = 1): term {
	return { item, coef: integer.from(coef) };
}
export function termAsSymbolic(t: term): symbolic {
	return t.coef === 1 ? t.item : t.item.scale(t.coef);
}


export function addTerms(num0: integer|rational, ...a: readonly Readonly<term>[]): symbolic {
	const terms: term[] = [];
	let num	= (real.is(num0) ? num0 : num0.num)  as integer;
	let den	= (real.is(num0) ? 1 : num0.den) as integer;

	function add(item: symbolic, coef: integer) {
		if (isConst(item)) {
			num = num + coef * item.value as integer;

		} else if (item instanceof symbolicAdd) {
			num = num + item.num * coef as integer;
			for (const j of item.terms)
				add(j.item, j.coef * coef as integer);

		} else if (item instanceof symbolicMul) {
			coef = coef * item.num.num as integer;

			if (!item.num.isInteger()) {
				const d = item.num.den;
				den = den * d as integer;
				num = num * d as integer;
				for (const i in terms)
					terms[i].coef = terms[i].coef * d as integer;
			}

			if (coef) {
				if (item.factors.length === 0) {
					num = num + coef as integer;
					return;
				}

				if (item.factors.length === 1 && item.factors[0].pow.is1()) {
					const inner = item.factors[0].item;
					if (inner instanceof symbolicAdd) {
						num = num + inner.num * coef as integer;
						for (const j of inner.terms)
							add(j.item, j.coef * coef as integer);
					} else {
						add(inner, coef);
					}
				} else {
					// pull numeric multiplier from mul into term coefficient
					terms.push(term(item.num.is1() ? item : symbolicMul.create(item.factors), coef));
				}
			}

		} else {
			terms.push(term(item, coef));
		}
	}

	for (const i of a)
		add(i.item, i.coef * den as integer);

	// canonical order
	terms.sort((a, b) => compare(a.item.id, b.item.id));

	// combine like terms by summing coefficients
	const combined: term[] = [];
	for (const t of terms) {
		const last = combined.at(-1);
		if (last && last.item.id === t.item.id)
			last.coef = last.coef + t.coef as integer;
		else
			combined.push(t);
	}

	// remove zero coefficients
	const nonzero = combined.filter(t => t.coef);

	if (nonzero.length === 0)
		return symbolic.from(num);

	// factor out GCD from term coefficients and constant
	const	coeffs	= nonzero.map(t => integer.abs(t.coef));
	if (num)
		coeffs.push(integer.abs(num));

	const	gcd		= integer.gcd(...coeffs);
//	if (nonzero[0].coef.sign() < 0)
//		g = g.neg();

	if (gcd !== 1) {
		for (const i in nonzero)
			nonzero[i].coef = nonzero[i].coef / gcd as integer;
		num = num / gcd as integer;
	}

	const result = nonzero.length === 1 && num === 0 ? termAsSymbolic(nonzero[0]) : symbolicAdd.create(nonzero, num);
	return gcd === den ? result
		: mulFactors(rational(gcd, den), factor(result));

}

export class symbolicAdd extends symbolic {
	static validate(terms: readonly Readonly<term>[], num: integer)  {
		if (terms.length < (num ? 1 : 2))
			throw new Error('not enough terms in additive expression');
		let prevId = '';
		for (const t of terms) {
			if (t.coef === 0)
				throw new Error('unexpected zero coefficient in term');
			if (t.item.id <= prevId)
				throw new Error('terms not in canonical order or duplicate term');
			if (t.item instanceof symbolicAdd) {
				//if (t.coef === 1 || terms.length === 1)
					throw new Error('unexpected additive term in term item');
			} else if (t.item instanceof symbolicMul) {
				if (!t.item.num.is1())
					throw new Error('unexpected numeric factor in term item');
				if (t.item.factors.length === 0 || (t.item.factors.length === 1 && t.item.factors[0].pow.is1()))
					throw new Error('unexpected factor in term item');
			}
			prevId = t.item.id;
		}
		// TODO: fix egraph rules that create fractional coefficients
		// if (terms.length > 1 && gcd2(...terms.map(t => Math.abs(t.coef))) !== 1)
		// 	throw new Error('term coefficients not in lowest terms');
	}
	static create(terms: readonly term[], num = 0 as integer) : symbolic	{
		this.validate(terms, num);
		return this.interner.intern(`a(${num === 0 ? '' : `${num},`}${terms.map(i => `${i.coef === 1 ? '' : i.coef === -1 ? '-' : i.coef}${i.item.id}`).join(',')})`, id => new symbolicAdd(id, terms, num));
	}

    constructor(id: string, public terms: readonly Readonly<term>[], public num: integer) {
		super(id);
	}
	neg(): symbolic {
		return	this.num === 0 && this.terms.length === 1 && Number(this.terms[0].coef) === -1 ? this.terms[0].item
			:	addTerms(-this.num as integer, ...this.terms.map(i => term(i.item, -i.coef)));
	}

	add(b: param): symbolic	{
		return  b instanceof symbolicAdd ? addTerms(this.num + b.num as integer, ...this.terms, ...b.terms)
			: 	addTerms(this.num, ...this.terms, term(asSymbolic(b)));
	}
	sub(b: param): symbolic	{ 
		return	b instanceof symbolicAdd ? addTerms(this.num - b.num as integer, ...this.terms, ...b.terms.map(i => term(i.item, -i.coef)))
			:	addTerms(this.num, ...this.terms, term(asSymbolic(b), -1));
	}

	match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
		const done = symbolic.matchLog(() => `add: ${this} vs ${node}`);
		try {
			if (node instanceof symbolicAdd) {
				if (opts?.exact && this.num !== node.num)
					return null;

				const opts2 = { ...opts, exact: true};
			
				// Backtracking matcher: try to pair each pattern term with a distinct node term such that all bindings remain consistent

				const tryMatch = (pIdx: number, nterms: readonly term[]): Bindings | null => {
					if (pIdx === this.terms.length) {
						// All pattern terms matched; check if we used all node terms or have unmatched rest.
						if (opts?.exact && nterms.length)
							return null;
						if (this.num !== node.num || nterms.length)
							bindings['_addrest'] = addTerms(this.num - node.num as integer, ...nterms);
						return bindings;
					}

					const pterm = this.terms[pIdx];
					// Try to match pterm against each remaining node term
					for (let i = 0; i < nterms.length; i++) {
						const nterm = nterms[i];
						if (pterm.coef === nterm.coef) {
							const saveBindings = { ...bindings };
							if (pterm.item.match(nterm.item, bindings, opts2)) {
								// Try to match remaining pattern terms with remaining node terms
								const remaining	= [...nterms.slice(0, i), ...nterms.slice(i + 1)];
								if (tryMatch(pIdx + 1, remaining))
									return bindings;
								bindings = {...saveBindings};
							}
						}
					}
					return null;
				};

				return tryMatch(0, node.terms);
			}
			return null;

		} finally {
			done();
		}
	}

	visit(visitor: Visitor, parent?: symbolic): symbolic {
		return visit(this,
			() => addTerms(this.num, ...this.terms.map((i) => term(i.item.visit(visitor, this), i.coef))), visitor, parent,
			() => this.terms.map(i => i.item)
		) as symbolic;
	}

	substitute(map: Bindings) : symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.substitute(map), i.coef)));
	}

	collect(v: string|symbolic): symbolic[] {
		const groups: symbolic[] = [symbolic.from(Number(this.num))];
		if (typeof v === 'string')
			v = symbolic.variable(v);

		for (const t of this.terms) {
			const i = t.item;
			if (i instanceof symbolicMul) {
				let pow = 0;
				const remaining: factor[] = [];
				for (const ff of i.factors as factor[]) {
					if (ff.item === v)
						pow += Number(ff.pow);
					else
						remaining.push(ff);
				}
				groups[pow] = (groups[pow] ??= zero).add(mulFactors(i.num.scale(t.coef), ...remaining));

			} else if (i === v) {
				groups[1] = (groups[1] ??= zero).add(symbolic.from(Number(t.coef)));

			} else {
				groups[0] = groups[0].add(t.item.scale(t.coef));
			}
		}
		return groups;
	}

	expand(opts?: ExpandOptions): symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.expand(opts), i.coef)));
	}

	derivative(v: string) : symbolic {
		return addTerms(0 as integer, ...this.terms.map(i => term(i.item.derivative(v), i.coef)));
	}
	integral(v: string) : symbolic {
		return addTerms(0 as integer, ...this.terms.map(i => term(i.item.integral(v), i.coef)), term(symbolicVariable.create(v), this.num));
	}
	evaluate(env?: Record<string, number>) : number {
		return this.terms.reduce((acc, curr) => {
			if (acc !== acc) 	// NaN check
				return acc;
			return acc + curr.coef * curr.item.evaluate(env);
		}, Number(this.num));
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return this.terms.reduce((acc, curr) => {
			if (!acc)
				return acc;
			const v = curr.item.evaluateT(ops, env);
			return v ? ops.add(acc, ops.mul(v, ops.from(Number(curr.coef)))) : v;
		}, ops.from(Number(this.num)) as T | undefined);
	}

	_toString(opts: StringifyOptions) : string {
		function termOrder(t: term): number {
			return t.item instanceof symbolicMul ? t.item.factors.reduce((acc, curr) => acc + Number(curr.pow), 0) : 0;
		}

		const terms = opts.sortTerms ? this.terms.slice().sort((a, b) => termOrder(b) - termOrder(a)) : this.terms;

		if (opts.direct) {
			return '(' + terms.map(t => printScaled(t.item._toString(opts), rational(t.coef), opts)).join(opts.addChar)
			+ (this.num === 0 ? '' : opts.addChar + opts.printConst(rational(this.num)))
			+ ')';
		}

		return terms.map((t, j) => {
			return	(j === 0 ? (Number(t.coef) < 0 ? '-' : '') : (Number(t.coef) < 0 ? opts.subChar : opts.addChar))
				+	printScaled(t.item._toString(opts), rational(t.coef).abs(), opts);
		}).join('')
		+ (this.num ? (Number(this.num) < 0 ? opts.subChar : opts.addChar) + opts.printConst(rational(this.num).abs()) : '');
	}
}

//-----------------------------------------------------------------------------
// mul
//-----------------------------------------------------------------------------

export interface factor {
	item:	symbolic;
	pow:	rational; 
}
export function factor(item: symbolic, pow = rational(1)): factor {
	return { item, pow };
}
export function factorAsSymbolic(f: factor): symbolic {
	return f.item.npow(f.pow);
}

export function mulFactors(num: rational, ...f: Readonly<factor>[]): symbolic {
	const factors: factor[] = [];

	function constpow(value: integer, pow: rational) {
		if (value === 1)
			return;

		// Extract whole powers only if pow is an integer
		if (pow.den === 1) {
			const scale = value ** Math.abs(pow.num);
			num = pow.num > 0 ? num.scale(scale) : num.mul(rational(1, scale));
		} else {
			// Extract perfect roots: if value = i^d, then value^(n/d) = i^n
			for (let i = 2, f = 2 ** pow.den; f <= value; f = ++i ** pow.den) {
				while (value % f === 0) {
					num		= num.scale(i ** pow.num);
					value	= value / f as integer;
				}
			}
			if (value !== 1)
				factors.push(factor(symbolic.from(value), pow));
		}
	}

	function add(item: symbolic, pow: rational) {
		if (isConst(item)) {
			constpow(item.value as integer, pow);

		} else if (item instanceof symbolicMul) {
			constpow(Math.abs(item.num.num) as integer, pow);
			constpow(item.num.den as integer, pow.neg());
			
			if (item.num.sign() < 0) {
				if (!(pow.den & 1))
					factors.push(factor(symbolic.i));
				else
					num = num.neg();
			}

			for (const j of item.factors)
				add(j.item, j.pow.mul(pow));

		} else if (item instanceof symbolicAdd) {
			const first = item.terms[0];
			if (item.terms.length === 1 && item.num === 0) {
				constpow(first.coef, pow);
				add(first.item, pow);
			} else if (item.num === 0 && first.coef < 0 && pow.isInteger()) {
				if (pow.num & 1)
					num = num.neg();
				add(item.neg(), pow);
			} else {
				factors.push(factor(item, pow));
			}

		} else {
			factors.push(factor(item, pow));
		}
	}

	for (const i of f)
		add(i.item, i.pow);

	if (num.is0())
		return zero;

	if (num.den === 0)
		return infinity;

	// canonical order
	factors.sort((a, b) => compare(a.item.id, b.item.id));

	// combine like factors by summing powers
	const combined: factor[] = [];
	for (const f of factors) {
		const last = combined.at(-1);
		if (last && last.item.id === f.item.id)
			last.pow = last.pow.add(f.pow);
		else
			combined.push(f);
	}

	// remove zero coefficients and handle powers of i
	const nonzero = combined.filter(t => {
		if (t.item === i) {
			const pow = Number(t.pow) & 3;
			if (pow & 2)
				num = num.scale(-1);
			t.pow = rational(pow % 2);
		}
		return t.pow.sign() !== 0;
	});

	// just a constant
	if (nonzero.length === 0 && num.isInteger() && num.sign() >= 0)
		return symbolicConstant.create(num.num);

	// just single factor
	if (nonzero.length === 1 && nonzero[0].pow.is1() && num.is1())
		return nonzero[0].item;

	return symbolicMul.create(nonzero, num);
}

export class symbolicMul extends symbolic {
	static validate(factors: readonly factor[], _num: rational)  {
		let prevId = '';
		for (const t of factors) {
			if (t.pow.is0())
				throw new Error('unexpected zero power in factor');
			if (t.item.id <= prevId)
				throw new Error('factors not in canonical order or duplicate factor');
			if (t.item instanceof symbolicMul)
				throw new Error('unexpected multiplicative factor in factor item');
			if (t.item instanceof symbolicConstant && t.pow.isInteger())
				throw new Error('unexpected constant');
			prevId = t.item.id;
		}
	}
	static create(factors: readonly factor[], num = rational(1)) 	{
		this.validate(factors, num);
		return this.interner.intern(`m(${num.is1() ? '' : `${num},`}${factors.map(i => `${i.item.id}${!i.pow.is1() ? i.pow : ''}`).join(',')})`, id => new symbolicMul(id, factors, num));
	}

	constructor(id: string, public factors: readonly Readonly<factor>[], public num: rational) {
		super(id);
	}
	
	neg(): symbolic {
		return symbolicMul.create(this.factors, this.num.neg());
	}
	recip(): symbolic {
		return symbolicMul.create(this.factors.map(i => factor(i.item, i.pow.scale(-1))), this.num.recip());
	}
	npow(b: number|rational): symbolic {
		const s = makeRat(b);
		if (s instanceof symbolic) {
			return this.pow(s);
		} else {
			if (s.is0())
				return one;
			if (s.is1())
				return this;
			return mulFactors(rational(1), factor(symbolic.from(this.num), s), ...this.factors.map(i => factor(i.item, i.pow.mul(s))));
		}
	}
	scale(b: number|rational): symbolic	{
		const s = makeRat(b);
		return	s instanceof symbolic ? this.mul(s)
			:	s.is0()	? zero
			:	s.is1()	? this
			:	this.num.mul(s).is1() && this.factors.length === 1 && this.factors[0].pow.is1() ? this.factors[0].item
			:	mulFactors(this.num.mul(s), ...this.factors);
	}

	mul(b: param): symbolic	{
		b = asNumeric(b);
		return !(b instanceof symbolic)  ? this.scale(b)
			: b instanceof symbolicMul ? mulFactors(this.num.mul(b.num), ...this.factors, ...b.factors)
			: mulFactors(this.num, ...this.factors, factor(b));
	}
	match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
		const done = symbolic.matchLog(() => `mul: ${this} vs ${node}`);
		try {
			if (node instanceof symbolicMul) {
				//if (opts?.exact && !this.num.eq(node.num))
				//	return null;

				// Backtracking matcher: try to pair each pattern term with a distinct node term such that all bindings remain consistent

				const opts2 	= { ...opts, exact: true};
				const tryMatch = (pIdx: number, nfactors: readonly factor[]): Bindings | null => {
					if (pIdx === this.factors.length) {
						// All pattern terms matched; check if we used all node terms or have unmatched rest.
						if (nfactors.length == 1) {
							const extra = asNumeric(nfactors[0].item);
							if (!(extra instanceof symbolic) && extra.is1())
								return bindings;
						}
						if (opts?.exact && nfactors.length)
							return null;
						if (!this.num.eq(node.num) || nfactors.length)
							bindings['_mulrest'] = mulFactors(rational(1), ...nfactors);
						return bindings;
					}

					const pfactor = this.factors[pIdx];
					// Try to match pterm against each remaining node term
					for (let i = 0; i < nfactors.length; i++) {
						const nfactor = nfactors[i];
						if (pfactor.pow.eq(nfactor.pow)) {
							const saveBindings = { ...bindings };
							if (pfactor.item.match(nfactor.item, bindings, opts2)) {
								// Try to match remaining pattern terms with remaining node terms
								const remaining = [...nfactors.slice(0, i), ...nfactors.slice(i + 1)];
								if (tryMatch(pIdx + 1, remaining))
									return bindings;
								bindings = {...saveBindings};
							}
						}
					}
					if (opts?.powerTest && opts.powerTest !== 'exact') {
						//try again with laxer pow check
						const multiple = opts.powerTest === 'multiple';
						for (let i = 0; i < nfactors.length; i++) {
							const nfactor = nfactors[i];
							const pow = nfactor.pow.div(pfactor.pow);
							if (!pow.is1() && (!multiple || pow.isInteger())) {
								const saveBindings = { ...bindings };
								if (pfactor.item.match(nfactor.item.npow(pow), bindings, opts2)) {
									// Try to match remaining pattern terms with remaining node terms
									const remaining = [...nfactors.slice(0, i), ...nfactors.slice(i + 1)];
									if (tryMatch(pIdx + 1, remaining))
										return bindings;
									bindings = {...saveBindings};
								}
							}
						}
					}
					return null;
				};

				return tryMatch(0, [...node.factors, factor(symbolic.from(node.num.div(this.num)))]);

			} else if (Number(this.num) === -1 && this.factors.length === 1 && this.factors[0].pow.is1() && this.factors[0].item.is('add')) {
				// Special case: match negation
				if (this.factors[0].item.match(node, bindings, opts)) {
					bindings['_mulrest'] = one.neg();
					return bindings;
				}
			}
			return null;
		} finally {
			done();
		}
	}
	substitute(map: Bindings) : symbolic {
		return mulFactors(this.num, ...this.factors.map(f => factor(f.item.substitute(map), f.pow)));
	}
	visit(visitor: Visitor, parent?: symbolic): symbolic {
		return visit(this,
			() => mulFactors(this.num, ...this.factors.map((f) => factor(f.item.visit(visitor, this), f.pow))), visitor, parent,
			() => this.factors.map(f => f.item),
		) as symbolic;
	}
	expand(opts?: ExpandOptions): symbolic {
		let factors = this.factors;

		// expansion: recursively expand child nodes per opts
		if ((opts?.depth ?? 2) > 1) {
			const opts2	= opts?.depth ? { ...opts, depth: opts.depth - 1 } : opts;
			factors		= factors.map(f => factor(f.item.expand(opts2), f.pow));
		}

		const uses = opts?.uses;
		const checkUse = !uses ? ()=>true
		: (terms: readonly Readonly<term>[]) => {
			for (const t of terms) {
				//if (t.item.is('var') && uses.has(t.item.name))
				//	return true;
				if (t.item.is('mul') && t.item.factors.some(f => f.item.is('var') && uses.has(f.item.name)))
					return true;
			}
			return false;
		};

		const additive: symbolicAdd[] = [];
		const others: factor[] = [];

		for (const f of factors) {
			const npow = Number(f.pow);
			if (f.item instanceof symbolicAdd && npow > 0 && (!opts?.maxPow || npow < opts.maxPow) && Number.isInteger(npow) && checkUse(f.item.terms)) {
				for (let i = 0; i < npow; i++)
					additive.push(f.item);
			} else {
				others.push(f);
			}
		}

		if (opts?.maxParts) {
			let totalParts = Math.max(others.length, 1);
			for (const a of additive)
				totalParts += totalParts * (a.terms.length + (a.num === 0 ? 0 : 1));

			if (totalParts > opts?.maxParts)
				return this; // Skip expansion to avoid combinatorial explosion
		}

		let parts: symbolic[] = [mulFactors(this.num, ...others)];
		for (const a of additive) {
			const parts2: symbolic[] = [];
			for (const p of parts) {
				for (const t of a.terms)
					parts2.push(p.mul(t.item).scale(t.coef));
				if (a.num !== 0)
					parts2.push(p.scale(a.num));
			}
			parts = parts2;
		}

		return addTerms(0 as integer, ...parts.map(p => term(p)));
	}

	derivative(v: string) : symbolic {
		return addTerms(0 as integer, ...this.factors.map(f => term(mulFactors(
			this.num.mul(f.pow),
			factor(f.item, f.pow.sub(rational(1))),
			factor(f.item.derivative(v)),
			...this.factors.filter(g => g !== f)
		))));
	}

	integral(x: string): symbolic {

		if (this.factors.length === 1) {
			const f = this.factors[0];
			// Power rule: ∫x^n dx = x^(n+1)/(n+1)
			if (f.item instanceof symbolicVariable && f.item.name === x) {
				if (!f.pow.eq(rational(-1))) {
					const newPow = f.pow.add(rational(1));
					return symbolicVariable.create(x).pow(newPow).mul(this.num).div(newPow);
				}
			}
		}


		// U-substitution: detect g'(x) * f(g(x)) pattern
		// For products with factors, check if some factors form the derivative of a function's inner argument
		for (const factor of this.factors) {
			if (factor.pow.is1() && (factor.item.is('unary') || factor.item.is('binary'))) {
				const res = factor.item.integralMul(x, mulFactors(this.num, ...this.factors.filter(f => f !== factor)));
				if (res)
					return res;
			}

			// Trig/hyperbolic power reduction for products: ∫k·dA·sin^n(A) dx where rest = k·dA
			if (factor.item.is('unary') && factor.pow.isInteger() && factor.pow.num >= 2) {
				const n		= factor.pow.num;
				const arg	= factor.item.arg;
				const dA	= arg.derivative(x);
				const rest	= mulFactors(this.num, ...this.factors.filter(f => f !== factor));
				const k		= checkLinearMul(rest, dA);	// check if rest = k·dA for constant k
				if (k) {
					switch (factor.item.name) {
						// ∫k·dA·sin^n(A) dx = k·[-sin^(n-1)(A)·cos(A)/n + (n-1)/n · ∫dA·sin^(n-2)(A) dx]
						case 'sin': {
							const term1 = symbolic.sin(arg).npow(n - 1).mul(symbolic.cos(arg)).div(-n);
							const term2 = dA.mul(symbolic.sin(arg).npow(n - 2)).integral(x).mul(rational(n - 1, n));
							return term1.add(term2).mul(k);
						}
						// ∫k·dA·cos^n(A) dx = k·[cos^(n-1)(A)·sin(A)/n + (n-1)/n · ∫dA·cos^(n-2)(A) dx]
						case 'cos': {
							const term1 = symbolic.cos(arg).npow(n - 1).mul(symbolic.sin(arg)).div(n);
							const term2 = dA.mul(symbolic.cos(arg).npow(n - 2)).integral(x).mul(rational(n - 1, n));
							return term1.add(term2).mul(k);
						}
						// ∫k·dA·sinh^n(A) dx = k·[sinh^(n-1)(A)·cosh(A)/n - (n-1)/n · ∫dA·sinh^(n-2)(A) dx]
						case 'sinh': {
							const term1 = symbolic.sinh(arg).npow(n - 1).mul(symbolic.cosh(arg)).div(n);
							const term2 = dA.mul(symbolic.sinh(arg).npow(n - 2)).integral(x).mul(rational(n - 1, n));
							return term1.sub(term2).mul(k);
						}
						// ∫k·dA·cosh^n(A) dx = k·[cosh^(n-1)(A)·sinh(A)/n + (n-1)/n · ∫dA·cosh^(n-2)(A) dx]
						case 'cosh': {
							const term1 = symbolic.cosh(arg).npow(n - 1).mul(symbolic.sinh(arg)).div(n);
							const term2 = dA.mul(symbolic.cosh(arg).npow(n - 2)).integral(x).mul(rational(n - 1, n));
							return term1.add(term2).mul(k);
						}
					}
				}
			}
		}

		// Integration by parts: ∫u·dv = u·v - ∫v·du
		if (this.factors.length > 1) {
			// Check trig×exp, hyp×exp, and trig product rules first
			for (const r of [...trigExpRules, ...hypExpRules, ...trigProdRules]) {
				const bs: Bindings = {wrt: symbolic.sym(x)};
				const res = r.match(this, bs, {});
				if (res && (!r.guard || r.guard(res)))
					return r.replace(res);
			}

			// Compute LIATE priority for each factor
			const priorities = this.factors.map(factor => ({factor, priority: liatePriority(factor.item)}));
			priorities.sort((a, b) => a.priority - b.priority);	// lower = better choice for u

			// Try u = highest priority factor, dv = rest
			const uFactor = priorities[0].factor;
			
			// only handle simple products for now
			if (uFactor.pow.is1()) {
				const u		= uFactor.item;
				const dv	= mulFactors(this.num, ...this.factors.filter(f => f !== uFactor));
				const v		= dv.integral(x);

				if (!(v instanceof symbolicIntegral)) {
					const du	= u.derivative(x);
					const vdu	= v.mul(du);

					// Recursive integral ∫v·du
					const vduIntegral = vdu.integral(x);

					// Check for tabular/reduction: if ∫v·du contains ∫u·dv, solve algebraically
					// For now, just check if result is simpler (not symbolicIntegral)
					if (!(vduIntegral instanceof symbolicIntegral))
						// ∫u·dv = u·v - ∫v·du
						return u.mul(v).sub(vduIntegral);
				}
			}
		}

		for (const r of integrationRules) {
			const bs: Bindings = {wrt: symbolic.sym(x)};
			const res = r.match(this, bs, {powerTest: 'multiple'});
			if (res && (!r.guard || r.guard(res)))
				return r.replace(res);
		}
/*
		// Try to recognize special forms: 1/(1±f²), 1/sqrt(1±f²), 1/f, 1/f², etc.
		if (this.factors.length === 1) {
			const f = this.factors[0];
			const pow = f.pow;

			// 1/f form -> log(f) if rest = f'/num
			if (pow.eq(rational(-1))) {
				const df = asNumeric(f.item.derivative(x));
				if (!(df instanceof symbolic) && !df.is0())
					// const/f -> log(f) * const / f'
					return symbolic.log(f.item).scale(this.num.div(df));
			}

			// 1/(a + b·f²)^n forms
			if (f.item.is('add')) {
				const add = f.item;
				// Look for pattern: a + b·g² where g is something we can integrate
				if (add.terms.length === 1 && add.terms[0].item.is('mul')) {
					const innerMul = add.terms[0].item;
					if (innerMul.factors.length === 1) {
						const innerF = innerMul.factors[0];
						if (innerF.pow.eq(rational(2))) {
							// Have: (a + b·g²)^pow where a = add.num, b = innerMul.num * add.terms[0].coef
							const a = add.num;
							const b = Number(innerMul.num) * add.terms[0].coef;
							const g = innerF.item;
							const dg = asNumeric(g.derivative(x));

							if (!(dg instanceof symbolic)) {
								const coef = this.num.div(dg);

								// 1/(1+g²) -> atan(g)
								if (pow.eq(rational(-1)) && a === 1 && b === 1)
									return symbolic.atan(g).scale(coef);

								// 1/(1-g²) -> atanh(g)
								if (pow.eq(rational(-1)) && a === 1 && b === -1)
									return symbolic.atanh(g).scale(coef);

								// 1/sqrt(1-g²) -> asin(g)
								if (pow.eq(rational(-1, 2)) && a === 1 && b === -1)
									return symbolic.asin(g).scale(coef);

								// 1/sqrt(g²+1) -> asinh(g)
								if (pow.eq(rational(-1, 2)) && a === 1 && b === 1)
									return symbolic.asinh(g).scale(coef);

								// 1/sqrt(g²-1) -> acosh(g)
								if (pow.eq(rational(-1, 2)) && a === -1 && b === 1)
									return symbolic.acosh(g).scale(coef);
							}
						}
					}
				}
			}

			// 1/f² where f = cos(g) or cosh(g)
			if (pow.eq(rational(-2)) && f.item.is('unary')) {
				const inner = f.item as unaryFunctionBase;
				const g = inner.arg;
				const dg = g.derivative(x);

				if (isConst(dg) && inner.is('unary')) {
					const coef = Number(this.num) / Number(dg);
					if (inner.name === 'cos')
						return symbolic.tan(g).scale(coef);
					if (inner.name === 'cosh')
						return symbolic.tanh(g).scale(coef);
				}
			}
		}
*/
		// Can't integrate - return unevaluated
		return symbolicIntegral.create(this, x);
	}

	evaluate(env?: Record<string, number>) : number {
		return this.factors.reduce((acc, curr) => {
			if (acc !== acc || acc === 0) 	// NaN check & early out for 0
				return acc;
			const v = curr.item.evaluate(env);
			return	curr.pow.is1() ? acc * v
				: 	curr.pow.eq(rational(-1)) ? acc / v
				: 	acc * v ** Number(curr.pow);
		}, Number(this.num));
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return this.factors.reduce((acc, curr) => {
			if (!acc)
				return acc;
			const v = curr.item.evaluateT(ops, env);
			if (!v)
				return v;
			return	curr.pow.is1() ? ops.mul(acc, v)
				:	curr.pow.eq(rational(-1)) ? ops.div(acc, v)
				:	ops.mul(acc, ops.rpow(v, curr.pow.num, curr.pow.den));
		}, ops.from(Number(this.num)) as T | undefined);
	}

	_toString(opts: StringifyOptions) : string {
		const factors	= opts.sortFactors ? this.factors.slice().sort((a, b) => Number(b.pow) - Number(a.pow)) : this.factors;

		function printTerm(item: symbolic, n: rational) {
			const s = maybeParentheses(item._toString(opts), opts);

			if (n.is1())
				return s;

			if (opts.ccode) {
				if (n === rational(1, 2))
					return `sqrt(${s})`;
				if (n.isInteger() && n.num < 3 && item.is('var'))
					return Array.from({length: n.num}, () => s).join(opts.mulChar);
				return `pow(${s}, ${opts.printConst(n)})`;
			}

			if (opts.radicalPower && n.num === 1) {
				const c = radicalChars[n.den];
				if (c)
					return `${c}${s}`;
			}

			if (opts.superPower)
				return `${s}${toSuperscript(opts.printConst(n))}`;
			return `${s}^${opts.printConst(n)}`;
		}

		if (factors.length && factors[0].pow.sign() < 0) {
			return opts.printConst(this.num) + factors.map(i =>
				(i.pow.sign() < 0 ? opts.divChar : opts.mulChar) + printTerm(i.item, i.pow.abs())
			).join('');
		} else {
			return printScaled(factors.map((i, j) =>
				(j === 0 ? '' : i.pow.sign() < 0 ? opts.divChar : opts.mulChar) + printTerm(i.item, i.pow.abs())
			).join(''), this.num, opts);
		}

	}

}

//-----------------------------------------------------------------------------
// special constants
//-----------------------------------------------------------------------------

function specialConstant(name: string, toString = name, value?: number) {
	const C = class extends symbolic {
		constructor()		{ super(name); symbolic.set(this, this); }
		evaluate(_env?: Record<string, number>): number 			{ return value ?? NaN; }
		evaluateT<T>(ops: Operators<T>, _env?: Record<string, T>)	{ return ops.variable(name); }
		_toString(): string	{ return toString; }
	};
	return C;
}

const i			= new (specialConstant('i', '𝑖'));
const e			= new (specialConstant('e', '𝑒', Math.E));
const pi		= new (specialConstant('pi', 'π', Math.PI));
const nan		= new (specialConstant('NaN'));

class symbolicInfinity extends specialConstant('infinity', '∞', Infinity) {
	add(_b: param): symbolic {
		return this;
	}
	mul(b: param): symbolic {
		b = asNumeric(b);
		return !(b instanceof symbolic) && b.is0() ? nan : this;
	}
}

const infinity: symbolic	= new symbolicInfinity;

symbolic.set(i.mul(i), one.neg());
symbolic.set(zero.recip(), infinity);


//-----------------------------------------------------------------------------
// functions
//-----------------------------------------------------------------------------

function checkLinearMul(a: symbolic, b: symbolic): rational | undefined {
	if (a === b)
		return rational(1);
	const n = asNumeric(a.div(b));
	return n instanceof symbolic ? undefined : n;
}

abstract class unaryFunctionBase extends symbolic {
	constructor(id: string, public arg: symbolic) { super(id); }
	create(i: symbolic)							{ return (this.constructor as any).create(i); }
	expand(opts?: ExpandOptions):	symbolic	{ return this.create(this.arg.expand(opts)); }
	abstract integralMul(v: string, rest: symbolic): symbolic | undefined;
	abstract name: string;
};

function unaryFunction(name: string,
	evaluate:	(arg: number) => number,
	derivative:	(arg: symbolic) => symbolic,
	antideriv?:	(arg: symbolic) => symbolic,
	toString =	(a: symbolic, opts: StringifyOptions) => `${name}(${a._toString(opts)})`
) {
	const C = class extends unaryFunctionBase {
		get name() { return name; }
		static create(i: symbolic) : symbolic	{
			return this.interner.intern(`${name}:${i.id}`, id => new C(id, i));
		}
		match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
			return node instanceof C && this.arg.match(node.arg, bindings, {...opts, exact: true}) ? bindings : null;
		}
		visit(visitor: Visitor, parent?: symbolic): symbolic {
			return visit(this,
				() => this.create(this.arg.visit(visitor, this)),
				visitor, parent,
				() => [this.arg]
			) as symbolic;
		}
		substitute(map: Bindings) : symbolic {
			return this.create(this.arg.substitute(map));
		}
		derivative(v: string): symbolic { return derivative(this.arg).mul(this.arg.derivative(v)); }
		integral(v: string): symbolic {
			if (antideriv) {
				const argDeriv = asNumeric(this.arg.derivative(v));
				if (!(argDeriv instanceof symbolic))
					return antideriv(this.arg).div(argDeriv);
			}
			return symbolicIntegral.create(this, v);
		}
		integralMul(v: string, rest: symbolic): symbolic | undefined {
			if (antideriv) {
				const argDeriv = this.arg.derivative(v);
				// Check if other factors equal the derivative (or a constant multiple)
				const n = checkLinearMul(rest, argDeriv);
				if (n)
					return antideriv(this.arg).mul(n);
			}
		}
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg.evaluate(env)); }
		evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) { return ops.func(name, [this.arg.evaluateT(ops, env)!]); }

		_toString(opts: StringifyOptions): string { return toString(this.arg, opts); }
	};

	return C;
}

abstract class binaryFunctionBase extends symbolic {
	constructor(id: string, public arg1: symbolic, public arg2: symbolic) { super(id); }
	create(i: symbolic, j: symbolic)		{ return (this.constructor as any).create(i, j); }
	expand(opts?: ExpandOptions): symbolic	{ return this.create(this.arg1.expand(opts), this.arg2.expand(opts)); }
	abstract integralMul(v: string, rest: symbolic): symbolic | undefined;
	abstract name: string;
};

function binaryFunction(name: string,
	evaluate: (a: number, b: number) => number,
	derivative: (a: symbolic, b: symbolic) => [symbolic, symbolic],
	antideriv?: (a: symbolic, b: symbolic, wrt: 0|1) => symbolic | undefined,	// wrt: which argument is varying
	toString = (a: symbolic, b: symbolic, opts: StringifyOptions) => `${name}(${a._toString(opts)}, ${b._toString(opts)})`
) {
	const C = class extends binaryFunctionBase {
		get name() { return name; }
		static create(i: symbolic, j: symbolic): symbolic	{
			return this.interner.intern(`${name}:${i.id}:${j.id}`, id => new C(id, i, j));
		}
		match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
			const opts2 = {...opts, exact: true};
			return node instanceof C && this.arg1.match(node.arg1, bindings, opts2) && this.arg2.match(node.arg2, bindings, opts2) ? bindings : null;
		}
		visit(visitor: Visitor, parent?: symbolic): symbolic {
			return visit(this,
				() => this.create(this.arg1.visit(visitor, this), this.arg2.visit(visitor, this)), visitor, parent,
				() => [this.arg1, this.arg2]
			) as symbolic;
		}
		substitute(map: Bindings) : symbolic {
			return this.create(this.arg1.substitute(map), this.arg2.substitute(map));
		}
		derivative(v: string): symbolic {
			const [d1, d2] = derivative(this.arg1, this.arg2);
			return d1.mul(this.arg1.derivative(v)).add(d2.mul(this.arg2.derivative(v)));
		}
		integral(v: string): symbolic {
			if (antideriv) {
				const d1 = this.arg1.derivative(v);
				const d2 = this.arg2.derivative(v);

				// Case 1: arg2 is constant w.r.t. v, arg1 has constant derivative
				if (d2 === symbolic.zero && !(asNumeric(d1) instanceof symbolic)) {
					const ad = antideriv(this.arg1, this.arg2, 0);	// antideriv w.r.t. first arg
					if (ad)
						return ad.div(d1);
				}
				// Case 2: arg1 is constant w.r.t. v, arg2 has constant derivative
				if (d1 === symbolic.zero && !(asNumeric(d2) instanceof symbolic)) {
					const ad = antideriv(this.arg1, this.arg2, 1);	// antideriv w.r.t. second arg
					if (ad)
						return ad.div(d2);
				}
			}
			return symbolicIntegral.create(this, v);
		}
		integralMul(v: string, rest: symbolic): symbolic | undefined {
			if (antideriv) {
				const d1 = this.arg1.derivative(v);
				const d2 = this.arg2.derivative(v);

				// Case 1: arg2 is constant, check if rest matches d(arg1)/dx
				if (d2 === symbolic.zero) {
					const ad = antideriv(this.arg1, this.arg2, 0);
					if (ad) {
						const n = checkLinearMul(rest, d1);
						if (n)
							return ad.mul(n);
					}
				}
				// Case 2: arg1 is constant, check if rest matches d(arg2)/dx
				if (d1 === symbolic.zero) {
					const ad = antideriv(this.arg1, this.arg2, 1);
					if (ad) {
						const n = checkLinearMul(rest, d2);
						if (n)
							return ad.mul(n);
					}
				}
			}
		}
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg1.evaluate(env), this.arg2.evaluate(env)); }
		evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) { return ops.func(name, [this.arg1.evaluateT(ops, env)!, this.arg2.evaluateT(ops, env)!]); }

		_toString(opts: StringifyOptions): string { return toString(this.arg1, this.arg2, opts); }
	};
	return C;
}

class symbolicIntegral extends symbolic {
	constructor(id: string, public arg: symbolic, public variable: string) {
		super(id);
	}

	static create(expr: symbolic, variable: string): symbolic {
		return this.interner.intern(`∫:${expr.id}:${variable}`, id => new symbolicIntegral(id, expr, variable));
	}

	match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
		return	node instanceof symbolicIntegral
			&&	node.variable === this.variable
			&&	this.arg.match(node.arg, bindings, { ...opts, exact: true })
			?	bindings : null;
	}

	visit(visitor: Visitor, parent?: symbolic): symbolic {
		return visit(this,
			() => symbolicIntegral.create(this.arg.visit(visitor, this), this.variable),
			visitor, parent,
			() => [this.arg]
		) as symbolic;
	}

	substitute(map: Bindings): symbolic {
		return symbolicIntegral.create(this.arg.substitute(map), this.variable);
	}

	derivative(v: string): symbolic {
		// Fundamental theorem of calculus: d/dx ∫f(x)dx = f(x)
		return v === this.variable ? this.arg : this.arg.derivative(v);
	}

	evaluate(_env?: Record<string, number>): number {
		// Numerical integration (e.g., Simpson's rule or similar)
		// This is complex - may return NaN or require external integration library
		return NaN;
	}

	_toString(opts: StringifyOptions): string {
		return `∫(${maybeParentheses(this.arg._toString(opts), opts)}d${this.variable}`;
	}
}

//-----------------------------------------------------------------------------
// symbolicBoolean - maintain in disjunctive normal form (DNF)
//-----------------------------------------------------------------------------

abstract class symbolicBoolean extends symbolicBase {
	abstract not(): symbolicBoolean;
	abstract and(b: symbolicBoolean): symbolicBoolean;
	abstract or(b: symbolicBoolean): symbolicBoolean;

	then(t: param2, f: param2): symbolic {
		return combinePartitions(asSymbolic2(f), partition(this, asSymbolic2(t)));
	}

	evaluate(_env?: Record<string, number>)						: boolean|undefined	{ return; }
	evaluateT<T>(_ops: Operators<T>, _env?: Record<string, T>) 	: boolean|undefined	{ return; }
	visit(visitor: Visitor, parent?: symbolicBase): symbolicBoolean { return super.visit(visitor, parent) as symbolicBoolean; }
}

class symbolicFalse extends symbolicBoolean {
	static create()	{ return this.interner.intern(`false`, id => new symbolicFalse(id)); }
	constructor(id: string) { super(id); }
	not()					{ return symTrue; }
	and(_b: symbolicBoolean){ return this; }
	or(b: symbolicBoolean)	{ return b; }
	evaluate(_env?: Record<string, number>)						{ return false; }
	evaluateT<T>(_ops: Operators<T>, _env?: Record<string, T>) 	{ return false; }
}

class symbolicTrue extends symbolicBoolean {
	static create()	{ return this.interner.intern(`true`, id => new symbolicTrue(id)); }
	constructor(id: string) { super(id); }
	not()					{ return symFalse; }
	and(b: symbolicBoolean)	{ return b; }
	or(_b: symbolicBoolean)	{ return this; }
	evaluate(_env?: Record<string, number>)						{ return true; }
	evaluateT<T>(_ops: Operators<T>, _env?: Record<string, T>) 	{ return true; }
}

const symTrue	= symbolicTrue.create();
const symFalse	= symbolicFalse.create();

const OpMasks = {
	FALSE:	0,
	EQ:		1,
	LT:		2,
	GT:		4,
	NE:		6,
	TRUE:	7,
};

const Ops = {
	'=':		OpMasks.EQ,
	'!=':		OpMasks.EQ ^ OpMasks.TRUE,
	'<':		OpMasks.LT,
	'>=':		OpMasks.LT ^ OpMasks.TRUE,
	'>':		OpMasks.GT,
	'<=':		OpMasks.GT ^ OpMasks.TRUE,
	'false':	OpMasks.FALSE,
	'true':		OpMasks.TRUE,
} as const;

type	Ops		= typeof Ops[keyof typeof Ops];
type	OpNames = keyof typeof Ops;
const	OpNames = Object.fromEntries(Object.entries(Ops).map(([k, v]) => [v, k]));

function reverseOp(op: Ops): Ops {
	return	(op & OpMasks.LT ? OpMasks.GT : 0)
		| 	(op & OpMasks.GT ? OpMasks.LT : 0)
		|	(op & OpMasks.EQ);
}

interface compareConsts {
	opL:	Ops;
	opH:	Ops;
	compL:	symbolicCompare;
	compH:	symbolicCompare;
}

export class symbolicCompare extends symbolicBoolean {
	static create(op: Ops, a: symbolic, b: symbolic)	{
		if (a.id > b.id)
			[a, b, op] = [b, a, reverseOp(op)];
		return this.interner.intern(`${OpNames[op]}:${a.id},${b.id}`, id => new symbolicCompare(id, a, b, op));
	}

	static consts(a: symbolicCompare, b: symbolicCompare): compareConsts | undefined {
		function inner(opA: Ops, opB: Ops, vA: number, vB: number) : compareConsts {
			return vA < vB
				? {opL: opA, opH: opB, compL: a, compH: b}
				: {opL: opB, opH: opA, compL: b, compH: a};
		}

		if (a.a === b.a) {
			if (isConst(a.b) && isConst(b.b))
				return inner(a.op, b.op, Number(a.b.value), Number(b.b.value));

		} else if (a.b === b.b) {
			if (isConst(a.a) && isConst(b.a))
				return inner(reverseOp(a.op), reverseOp(b.op), Number(a.a.value), Number(b.a.value));
		}
	}

	static andConsts(c: compareConsts): symbolicBoolean | undefined {
		// Equalities: if one is EQ, check if value satisfies the other
		if (c.opL === OpMasks.EQ)
			return c.opH & OpMasks.LT ? c.compL : symFalse;
		if (c.opH === OpMasks.EQ)
			return c.opL & OpMasks.GT ? c.compH : symFalse;

		// Inequalities: != with any bound returns the bound
		if (c.opL === OpMasks.NE)
			return c.compH;
		if (c.opH === OpMasks.NE)
			return c.compL;

		const opO = c.opL | c.opH;

		// Same direction: take the stronger bound
		if (!(opO & OpMasks.GT))
			return c.compL; // both ≤: take min
		if (!(opO & OpMasks.LT))
			return c.compH; // both ≥: take max

		// Opposite directions: check if satisfiable
		if (!(c.opL & c.opH & ~OpMasks.EQ))
			return symFalse; // strict bounds don't meet
	}

	static orConsts(c: compareConsts): symbolicBoolean | undefined {
		// Equalities: check if one subsumes the other
		if (c.opL === OpMasks.EQ)
			return c.opH & OpMasks.LT ? c.compH : c.compL;
		if (c.opH === OpMasks.EQ)
			return c.opL & OpMasks.GT ? c.compL : c.compH;

		// Inequalities: often cover everything or subsume the other
		if (c.opL === OpMasks.NE)
			return c.opH & OpMasks.LT ? symTrue : c.compL;
		if (c.opH === OpMasks.NE)
			return c.opL & OpMasks.GT ? symTrue : c.compH;

		const opO = c.opL | c.opH;

		// Same direction: take the weaker bound
		if (!(opO & OpMasks.GT))
			return c.compH; // both ≤: take max
		if (!(opO & OpMasks.LT))
			return c.compL; // both ≥: take min

		// Opposite directions: usually covers everything
		if ((c.opL & OpMasks.GT) && (c.opH & OpMasks.LT))
			return symTrue;
	}

	constructor(id: string, public a: symbolic, public b: symbolic, public op: Ops) {
		super(id);
	}
	not(): symbolicBoolean {
		return symbolicCompare.create(this.op ^ OpMasks.TRUE, this.a, this.b);
	}

	and(b: symbolicBoolean): symbolicBoolean {
		if (!(b instanceof symbolicCompare))
			return b.and(this);

		if (this.a === b.a && this.b === b.b) {
			const opA = this.op & b.op;
			return opA ? symbolicCompare.create(opA, this.a, this.b) : symFalse;
		}

		const consts = symbolicCompare.consts(this, b);
		return (consts && symbolicCompare.andConsts(consts)) || symbolicAnd.create([this, b]);
	}
	or(b: symbolicBoolean): symbolicBoolean {
		if (!(b instanceof symbolicCompare))
			return b.or(this);

		if (this.a === b.a && this.b === b.b) {
			const opO = this.op | b.op;
			return opO === OpMasks.TRUE ? symTrue : symbolicCompare.create(opO, this.a, this.b);
		}

		const consts = symbolicCompare.consts(this, b);
		return (consts && symbolicCompare.orConsts(consts)) || symbolicOr.create([this, b]);
	}

	evaluate(env?: Record<string, number>) {
		const a = this.a.evaluate(env);
		const b = this.b.evaluate(env);
		switch (this.op) {
			case Ops['=']:	return a === b;
			case Ops['!=']:	return a !== b;
			case Ops['<']:	return a < b;
			case Ops['<=']:	return a <= b;
			case Ops['>']:	return a > b;
			case Ops['>=']:	return a >= b;
		}
	}

	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		const a = this.a.evaluateT(ops, env);
		if (!a)
			return;
		const b = this.b.evaluateT(ops, env);
		if (!b)
			return;
		switch (this.op) {
			case Ops['=']:	return ops.eq(a, b);
			case Ops['!=']:	return !ops.eq(a, b);
			case Ops['<']:	return ops.lt(a, b);
			case Ops['<=']:	return !ops.lt(b, a);
			case Ops['>']:	return ops.lt(b, a);
			case Ops['>=']:	return !ops.lt(a, b);
		}
	}

	_toString(opts: StringifyOptions): string {
		return `${this.a._toString(opts)} ${OpNames[this.op]} ${this.b._toString(opts)}`;
	}
}

class symbolicAnd extends symbolicBoolean {
	static create(terms: symbolicBoolean[])	{
		terms.sort((a, b) => compare(a.id, b.id));
		return this.interner.intern(`&:${terms.map(t => t.id).join(',')}`, id => new symbolicAnd(id, terms));
	}
	constructor(id: string, public terms: symbolicBoolean[]) {
		super(id);
	}
	not(): symbolicBoolean {
		return symbolicOr.create(this.terms.map(t => t.not()));
	}
	and(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicAnd	? symbolicAnd.create([...this.terms, ...b.terms])
			:	b instanceof symbolicOr		? symbolicOr.create(b.terms.map(t => this.and(t)))
			:	symbolicAnd.create([this, b]);
	}
	or(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicAnd	? symbolicOr.create([b, this])
			:	b instanceof symbolicOr		? symbolicOr.create(b.terms.map(t => this.or(t)))
			:	symbolicOr.create([this, b]);
	}
	_toString(opts: StringifyOptions): string {
		return `(${this.terms.map(t => t._toString(opts)).join(' && ')})`;
	}
}
class symbolicOr extends symbolicBoolean {
	static create(terms: symbolicBoolean[])	{
		terms.sort((a, b) => compare(a.id, b.id));
		return this.interner.intern(`|:${terms.map(t => t.id).join(',')}`, id => new symbolicOr(id, terms));
	}
	constructor(id: string, public terms: symbolicBoolean[]) {
		super(id);
	}
	not(): symbolicBoolean {
		return symbolicAnd.create(this.terms.map(t => t.not()));
	}
	and(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicAnd	? symbolicOr.create(this.terms.map(t => t.and(b)))
			:	b instanceof symbolicOr		? symbolicOr.create(this.terms.flatMap(t1 => b.terms.map(t2 => t1.and(t2))))
			:	symbolicOr.create(this.terms.map(t => t.and(b)));
	}
	or(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicAnd	? symbolicOr.create([...this.terms, b])
			:	b instanceof symbolicOr		? symbolicOr.create([...this.terms, ...b.terms])
			:	symbolicOr.create([...this.terms, b]);
	}
	_toString(opts: StringifyOptions): string {
		return `(${this.terms.map(t => t._toString(opts)).join(' || ')})`;
	}
}

//-----------------------------------------------------------------------------
// partition (switch statement)
//-----------------------------------------------------------------------------

interface partition {
	bool:	symbolicBoolean;
	value:	symbolic;
}
function partition(bool: symbolicBoolean, value: symbolic): partition {
	return { bool, value };
}

export function combinePartitions(otherwise: symbolic, ...p: readonly Readonly<partition>[]): symbolic {
	const partitions: partition[] = [];
	const trues: symbolic[] = [];

	if (otherwise instanceof symbolicPartition) {
		partitions.push(...otherwise.partitions);
		otherwise = otherwise.otherwise;
	}

	for (const i of p) {
		if (i.value instanceof symbolicPartition) {
			i.value.partitions.forEach(sub => {
				partitions.push(partition(i.bool.and(sub.bool), sub.value));
			});
			partitions.push(partition(i.bool, i.value.otherwise));
		} else {
			partitions.push(partition(i.bool, i.value));	// don't use original (?)
		}
	}

	// canonical order
	partitions.sort((a, b) => compare(a.value.id, b.value.id));

		// combine like terms by summing coefficients
	const combined: partition[] = [];
	for (const t of partitions) {
		const last = combined.at(-1);
		if (last && last.value === t.value)
			last.bool = last.bool.or(t.bool);
		else
			combined.push(t);
	}

	// remove zero coefficients
	const nonzero = combined.filter(t => {
		if (t.bool === symTrue) {
			trues.push(t.value);
			return false;
		}
		return t.bool !== symFalse;
	});

	if (trues.length > 0) {
		if (!trues.every(v => v === trues[0]))
			throw new Error('Conflicting partition true values');
		return trues[0];
	}

	if (nonzero.length === 0)
		return otherwise;

	return symbolicPartition.create(nonzero, otherwise);
}

class symbolicPartition extends symbolic {

	static create(partitions: readonly Readonly<partition>[], otherwise: symbolic)	{
		return this.interner.intern(`p(${partitions.map(i => `${i.bool.id}?${i.value.id}`).join(':')}:${otherwise.id})`, id => new symbolicPartition(id, partitions, otherwise));
	}

	constructor(id: string, public partitions: readonly Readonly<partition>[], public otherwise: symbolic) {
		super(id);
	}

	add(b: param): symbolic {
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.add(b))), this.otherwise.add(b));
	}
	sub(b: param): symbolic {
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.sub(b))), this.otherwise.sub(b));
	}
	scale(b: rational): symbolic	{
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.scale(b))), this.otherwise.scale(b));
	}
	mul(b: param): symbolic {
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.mul(b))), this.otherwise.mul(b));
	}
	div(b: param): symbolic {
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.div(b))), this.otherwise.div(b));
	}

	visit(visitor: Visitor, parent?: symbolic):		symbolic	{
		return visit(this,
			() => symbolicPartition.create(this.partitions.map(p => partition(p.bool.visit(visitor, this), p.value.visit(visitor, this))), this.otherwise.visit(visitor, this)),
			visitor, parent,
			() => this.partitions.map(p => [p.bool, p.value]).flat().concat(this.otherwise)
		) as symbolic;
	}
	evaluate(env?: Record<string, number>) :	number			{
		for (const p of this.partitions) {
			if (p.bool.evaluate(env))
				return p.value.evaluate(env);
		}
		return this.otherwise.evaluate(env);
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) : T|undefined	{
		for (const p of this.partitions) {
			if (p.bool.evaluateT(ops, env))
				return p.value.evaluateT(ops, env);
		}
		return this.otherwise.evaluateT(ops, env);
	}
	derivative(v: string):						symbolic		{
		return symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.derivative(v))), this.otherwise.derivative(v));
	}
	_toString(opts: StringifyOptions):			string			{
		return '(' + this.partitions.map(p => 
			`${p.bool.toString(opts)} ? ${p.value.toString(opts)}`
		).join(' : ') + ' : ' + this.otherwise.toString(opts) + ')';
	}

}

//-----------------------------------------------------------------------------
// functions
//-----------------------------------------------------------------------------

// basic

const symbolicAbs	= unaryFunction('abs', Math.abs,
	a => symbolicSign.create(a),
	a => a.mul(symbolicAbs.create(a)).div(2),
	(a, opts) => `|${a._toString(opts)}|`
);
const symbolicFloor	= unaryFunction('floor', Math.floor,
	_a => zero,
	a => a.mul(symbolicFloor.create(a)).sub(symbolicFloor.create(a).mul(symbolicFloor.create(a).add(one)).scale(0.5)),
	(a, opts) => `⌊${a._toString(opts)}⌋`
);
const symbolicCeil	= unaryFunction('ceil', Math.ceil,
	_a => zero,
	a => a.mul(symbolicCeil.create(a)).sub(symbolicCeil.create(a).mul(symbolicCeil.create(a).sub(one)).scale(0.5)),
	(a, opts) => `⌈${a._toString(opts)}⌉`
);
const symbolicTrunc	= unaryFunction('trunc', Math.trunc,
	_a => zero,
	a => a.mul(symbolicTrunc.create(a)).sub(symbolicTrunc.create(a).mul(symbolicTrunc.create(a).sub(one)).scale(0.5))
);
const symbolicRound	= unaryFunction('round', Math.round,
	_a => zero,
	a => a.mul(symbolicRound.create(a)).sub(symbolicRound.create(a).mul(symbolicRound.create(a).sub(one)).scale(0.5))
);

const symbolicMod	= binaryFunction('mod', (a, b) => a % b,
	(a, b) => [one, symbolicFloor.create(a.div(b)).neg()],
	(a, b) => a.sub(b.mul(symbolicFloor.create(a.div(b)))).pow(2).mul(symbolicSign.create(symbolicMod.create(a, b).sub(b.div(2))).div(2))
);
const symbolicSign	= unaryFunction('sign', Math.sign,
	_a => zero,
	a => symbolicAbs.create(a)
);
const symbolicFrac	= unaryFunction('frac', a => a - Math.floor(a),
	_a => one,
	a => a.pow(2).div(2).sub(a.mul(symbolicFloor.create(a))).add(symbolicFloor.create(a).mul(symbolicFloor.create(a).add(one)).div(2))
);

// min/max implemented via abs to keep expressions smooth and composable:
// min(a,b) = 0.5*(a + b - |a - b|)
// max(a,b) = 0.5*(a + b + |a - b|)
const symbolicMin = binaryFunction('min', (a, b) => Math.min(a, b),
	(a, b) => [
		one.sub(symbolicSign.create(a.sub(b))).scale(0.5),
		one.add(symbolicSign.create(a.sub(b))).scale(0.5)
	],
	(a, b) => a.integral('x').add(b.integral('x')).sub(symbolicAbs.create(a.sub(b)).integral('x')).scale(0.5));

const symbolicMax = binaryFunction('max', (a, b) => Math.max(a, b),
	(a, b) => [
		one.add(symbolicSign.create(a.sub(b))).scale(0.5),
		one.sub(symbolicSign.create(a.sub(b))).scale(0.5)
	],
	(a, b) => a.integral('x').add(b.integral('x')).add(symbolicAbs.create(a.sub(b)).integral('x')).scale(0.5)
);

// complex

const symbolicRe	= unaryFunction('re', a => a, _a => one, undefined, (a, opts) => `re(${a._toString(opts)})`);
const symbolicIm	= unaryFunction('im', _a => 0, _a => one, undefined, (a, opts) => `im(${a._toString(opts)})`);


// trigonometric

const symbolicSin	= unaryFunction('sin', Math.sin, a => symbolicCos.create(a), a => symbolicCos.create(a).neg(), (a, opts) => `sin(${a._toString(opts)})`);
const symbolicCos	= unaryFunction('cos', Math.cos, a => symbolicSin.create(a).neg(), a => symbolicSin.create(a), (a, opts) => `cos(${a._toString(opts)})`);
const symbolicTan	= unaryFunction('tan', Math.tan, a => symbolicCos.create(a).npow(-2), a => symbolicLog.create(symbolicAbs.create(symbolicCos.create(a))).neg(), (a, opts) => `tan(${a._toString(opts)})`);

const symbolicAsin	= unaryFunction('asin', Math.asin, a => one.sub(a.npow(2)).rpow(-1, 2), a => a.mul(symbolicAsin.create(a)).add(one.sub(a.npow(2)).rpow(1, 2)));
const symbolicAcos	= unaryFunction('acos', Math.acos, a => one.sub(a.npow(2)).rpow(-1, 2).neg(), a => a.mul(symbolicAcos.create(a)).sub(one.sub(a.npow(2)).rpow(1, 2)));
const symbolicAtan	= unaryFunction('atan', Math.atan, a => one.div(a.npow(2).add(one)), a => a.mul(symbolicAtan.create(a)).sub(symbolicLog.create(one.add(a.npow(2))).scale(0.5)));

const symbolicAtan2 = binaryFunction('atan2',
	Math.atan2,
	(a, b) => {
		const denom = b.npow(2).add(a.npow(2));
		return [ b.div(denom), a.mul(symbolic.from(-1)).div(denom) ];
	},
	(a, b, wrt) => {
		// ∫atan2(y, c) dy = y·atan2(y, c) - (c/2)·ln(y² + c²)
		// ∫atan2(c, x) dx = x·atan2(c, x) + (c/2)·ln(x² + c²)
		const r2 = a.npow(2).add(b.npow(2));
		return wrt === 0
			? a.mul(symbolicAtan2.create(a, b)).sub(b.mul(symbolicLog.create(r2)).scale(0.5))
			: b.mul(symbolicAtan2.create(a, b)).add(a.mul(symbolicLog.create(r2)).scale(0.5));
	}
);

symbolic.set(symbolic.sin(zero), zero);
symbolic.set(symbolic.sin(pi.scale(0.5)), one);
symbolic.set(symbolic.sin(pi), zero);

symbolic.set(symbolic.cos(zero), one);
symbolic.set(symbolic.cos(pi.scale(0.5)), zero);
symbolic.set(symbolic.cos(pi), one.neg());

symbolic.set(symbolic.tan(zero), zero);
symbolic.set(symbolic.tan(pi.scale(0.25)), one);

symbolic.set(symbolic.asin(zero), zero);
symbolic.set(symbolic.asin(one), pi.scale(0.5));

symbolic.set(symbolic.acos(zero), pi.scale(0.5));
symbolic.set(symbolic.acos(one), zero);

symbolic.set(symbolic.atan(zero), zero);
symbolic.set(symbolic.atan(one), pi.scale(0.25));

// exponential and logarithm

const symbolicExp = unaryFunction('exp', Math.exp, arg => symbolic.exp(arg), arg => symbolic.exp(arg), (a, opts) => `exp(${a._toString(opts)})`);
const symbolicLog = unaryFunction('log', Math.log, arg => arg.recip(), arg => arg.mul(symbolicLog.create(arg).sub(1)), (a, opts) => `log(${a._toString(opts)})`);

const symbolicPow = binaryFunction('pow',
	Math.pow,
	(a, b) => [b.mul(a.pow(b.sub(one))), symbolic.log(a).mul(a.pow(b))] as [symbolic, symbolic],
	(a, b, wrt) => {
		if (wrt === 0) {
			// ∫a^n da = a^(n+1)/(n+1) when n is constant (power rule)
			const n = asNumeric(b);
			if (!(n instanceof symbolic) && !n.eq(rational(-1)))
				return symbolicPow.create(a, b.add(one)).div(b.add(one));
		} else {
			// ∫n^b db = n^b / ln(n) when n is constant (exponential rule)
			const n = asNumeric(a);
			if (!(n instanceof symbolic))
				return symbolicPow.create(a, b).div(symbolicLog.create(a));
		}
	},
	(a, b, opts) => opts.ccode ? `pow(${a._toString(opts)}, ${b._toString(opts)})` : `(${a._toString(opts)}) ^ (${b._toString(opts)})`
);
symbolic.set(symbolic.exp(one), e);
symbolic.set(symbolic.log(e), one);

// hyperbolic

const symbolicSinh = unaryFunction('sinh', Math.sinh, a => symbolicCosh.create(a), a => symbolicCosh.create(a));
const symbolicCosh = unaryFunction('cosh', Math.cosh, a => symbolicSinh.create(a), a => symbolicSinh.create(a));
const symbolicTanh = unaryFunction('tanh', Math.tanh, a => symbolicCosh.create(a).npow(-2), a => symbolicLog.create(symbolicCosh.create(a)));

const symbolicAsinh = unaryFunction('asinh', Math.asinh, a => a.npow(2).add(one).rpow(-1, 2), a => a.mul(symbolicAsinh.create(a)).sub(a.npow(2).add(one).rpow(1, 2)));
const symbolicAcosh = unaryFunction('acosh', Math.acosh, a => a.npow(2).sub(one).rpow(-1, 2), a => a.mul(symbolicAcosh.create(a)).sub(a.npow(2).sub(one).rpow(1, 2)));
const symbolicAtanh = unaryFunction('atanh', Math.atanh, a => one.sub(a.npow(2)).recip(), a => a.mul(symbolicAtanh.create(a)).add(symbolicLog.create(one.sub(a.npow(2))).scale(0.5)));

symbolic.set(symbolic.sinh(zero), zero);
symbolic.set(symbolic.cosh(zero), one);
symbolic.set(symbolic.tanh(zero), zero);

symbolic.set(symbolic.asinh(zero), zero);
symbolic.set(symbolic.acosh(one), zero);
symbolic.set(symbolic.atanh(zero), zero);

//-----------------------------------------------------------------------------

const types = {
	const:		symbolicConstant,
	var:		symbolicVariable,
	add:		symbolicAdd,
	mul: 		symbolicMul,
	unary:		unaryFunctionBase,
	binary:		binaryFunctionBase,
	partition:	symbolicPartition,
	integral:	symbolicIntegral,
	compare:	symbolicCompare,
	and:		symbolicAnd,
	or:			symbolicOr,
	//sin: 		symbolicSin,
	//cos: 		symbolicCos,
	//tan: 		symbolicTan,
	//asin: 	symbolicAsin,
	//acos: 	symbolicAcos,
	//atan: 	symbolicAtan,
	//atan2: 	symbolicAtan2,
	//exp: 		symbolicExp,
	//log: 		symbolicLog,
	//sinh: 	symbolicSinh,
	//cosh: 	symbolicCosh,
	//tanh: 	symbolicTanh,
	//asinh: 	symbolicAsinh,
	//acosh: 	symbolicAcosh,
	//atanh: 	symbolicAtanh,
	//pow: 	symbolicPow
} as const;


//-----------------------------------------------------------------------------
// integration rules
//-----------------------------------------------------------------------------

const A		= symbolic.bind('A');
const B		= symbolic.bind('B');
const dA	= A.derivative('wrt');
const dB	= B.derivative('wrt');

export type IntegrationRule = {
	name:		string;
	match:		(node: symbolic, bs: Bindings, opts?: MatchOptions) => Bindings | null;
	replace:	(bs: Bindings) => symbolic;
	guard?:		(bs: Bindings, context?: any) => boolean;
};

function PatternRule(name: string, pattern: symbolic, replace: (bs: Bindings) => symbolic, guard?: (bs: Bindings, context: any) => boolean): IntegrationRule {
	return {
		name,
		match: (node, bs, opts) => pattern.match(node, bs, opts),
		replace: bs => {
			return replace(bs).mul(bs._mulrest ? bs._mulrest : one);
		},
		guard: (bs, context) => {
			if (guard && !guard(bs, context))
				return false;
			return !bs._mulrest || !(asNumeric(bs._mulrest) instanceof symbolic);
		}
	};
}

function checkdA(bs: Bindings): boolean {
	const v = varName(bs['wrt']);
	const dA = bs.A.derivative(v);
	const mulrest = bs._mulrest ?? one;

	if (dA === mulrest) {
		delete bs._mulrest;
		return true;
	}
	const ratio = mulrest.div(dA);
	if (asNumeric(ratio) instanceof symbolic)
		return false;

	bs._mulrest = ratio;
	return true;
}

export const integrationRules: IntegrationRule[] = [
	// Logarithmic
	// log: dA / A → log(A)
	PatternRule('log',
		one.div(A),
		bs => symbolic.log(bs.A),
		checkdA
	),

	// Inverse trig
	// asin: dA / sqrt(1 - A²) → asin(A)
	PatternRule('asin',
		one.div(one.sub(A.npow(2)).rpow(1, 2)),
		bs => symbolic.asin(bs.A),
		checkdA
	),
	// atan: dA / (1 + A²) → atan(A)
	PatternRule('atan',
		one.div(one.add(A.npow(2))),
		bs => symbolic.atan(bs.A),
		checkdA
	),
	// atan2: (A·dB - B·dA) / (A² + B²) → atan2(A, B)
	PatternRule('atan2',
		A.mul(dB).sub(B.mul(dA)).div(A.npow(2).add(B.npow(2))),
		bs => symbolic.atan2(bs.A, bs.B),
	),

	// Inverse hyperbolic
	// asinh: dA / sqrt(A² + 1) → asinh(A)
	PatternRule('asinh',
		one.div(A.npow(2).add(one).rpow(1, 2)),
		bs => symbolic.asinh(bs.A),
		checkdA
	),
	// acosh: dA / sqrt(A² - 1) → acosh(A)
	PatternRule('acosh',
		one.div(A.npow(2).sub(one).rpow(1, 2)),
		bs => symbolic.acosh(bs.A),
		checkdA
	),
	// atanh: dA / (1 - A²) → atanh(A)
	PatternRule('atanh',
		one.div(one.sub(A.npow(2))),
		bs => symbolic.atanh(bs.A),
		checkdA
	),

	// Trig
	// tan: sec²(A) · dA = dA / cos²(A) → tan(A)
	PatternRule('tan',
		one.div(symbolic.cos(A).npow(2)),
		bs => symbolic.tan(bs.A),
		checkdA
	),
	// tan²: dA · tan²(A) → tan(A) - A (tan² uses different formula than sin²/cos²)
	PatternRule('tan²',
		symbolic.tan(A).npow(2),
		bs => symbolic.tan(bs.A).sub(bs.A),
		checkdA
	),

	// Reciprocal trig (cot, csc, sec as 1/tan, 1/sin, 1/cos)
	// cot: dA / tan(A) → log|sin(A)|
	PatternRule('cot',
		one.div(symbolic.tan(A)),
		bs => symbolic.log(symbolic.sin(bs.A).abs()),
		checkdA
	),
	// csc: dA / sin(A) → log|(1 - cos(A)) / sin(A)|
	PatternRule('csc',
		one.div(symbolic.sin(A)),
		bs => symbolic.log(one.sub(symbolic.cos(bs.A)).div(symbolic.sin(bs.A)).abs()),
		checkdA
	),
	// sec: dA / cos(A) → log|(1 + sin(A)) / cos(A)|
	PatternRule('sec',
		one.div(symbolic.cos(A)),
		bs => symbolic.log(one.add(symbolic.sin(bs.A)).div(symbolic.cos(bs.A)).abs()),
		checkdA
	),

	// Hyperbolic
	// tanh: sech²(A) · dA = dA / cosh²(A) → tanh(A)
	PatternRule('tanh',
		one.div(symbolic.cosh(A).npow(2)),
		bs => symbolic.tanh(bs.A),
		checkdA
	),
	// coth: dA / tanh(A) → log|sinh(A)|
	PatternRule('coth',
		one.div(symbolic.tanh(A)),
		bs => symbolic.log(symbolic.sinh(bs.A).abs()),
		checkdA
	),
	// csch: dA / sinh(A) → log|tanh(A/2)|
	PatternRule('csch',
		one.div(symbolic.sinh(A)),
		bs => symbolic.log(symbolic.tanh(bs.A.div(2)).abs()),
		checkdA
	),
	// sech: dA / cosh(A) → atan(sinh(A))
	PatternRule('sech',
		one.div(symbolic.cosh(A)),
		bs => symbolic.atan(symbolic.sinh(bs.A)),
		checkdA
	),
	// tanh²: dA · tanh²(A) → A - tanh(A) (tanh² uses different formula than sinh²/cosh²)
	PatternRule('tanh²',
		symbolic.tanh(A).npow(2),
		bs => bs.A.sub(symbolic.tanh(bs.A)),
		checkdA
	),
];

// Guard for trig×exp patterns: checks dA and dB are constants, computes scale factor
function checkTrigExp(bs: Bindings): boolean {
	const v = varName(bs['wrt']);
	const dA = bs.A.derivative(v);
	const dB = bs.B.derivative(v);
	if (asNumeric(dA) instanceof symbolic || asNumeric(dB) instanceof symbolic)
		return false;
	const ratio = (bs._mulrest ?? one).div(dA.mul(dA).add(dB.mul(dB)));
	//if (asNumeric(ratio) instanceof symbolic)
	//	return false;
	bs.dA = dA;
	bs.dB = dB;
	bs._mulrest = ratio;
	return true;
}

// Trig × Exp products (these cycle infinitely under integration by parts)
export const trigExpRules: IntegrationRule[] = [
	// sin×exp: sin(A)·exp(B) → exp(B)/(a²+b²)·(b·sin(A) - a·cos(A))
	PatternRule('sin×exp',
		symbolic.sin(A).mul(symbolic.exp(B)),
		bs => symbolic.exp(bs.B).mul(symbolic.sin(bs.A).mul(bs.dB).sub(symbolic.cos(bs.A).mul(bs.dA))),
		checkTrigExp
	),
	// cos×exp: cos(A)·exp(B) → exp(B)/(a²+b²)·(a·sin(A) + b·cos(A))
	PatternRule('cos×exp',
		symbolic.cos(A).mul(symbolic.exp(B)),
		bs => symbolic.exp(bs.B).mul(symbolic.sin(bs.A).mul(bs.dA).add(symbolic.cos(bs.A).mul(bs.dB))),
		checkTrigExp
	),
];

// Guard for hyperbolic×exp patterns: denominator is b²-a² (not b²+a²)
function checkHypExp(bs: Bindings): boolean {
	const v = varName(bs['wrt']);
	const dA = bs.A.derivative(v);
	const dB = bs.B.derivative(v);
	if (asNumeric(dA) instanceof symbolic || asNumeric(dB) instanceof symbolic)
		return false;
	const denom = dB.mul(dB).sub(dA.mul(dA));
	if (denom === zero)
		return false;
	const ratio = (bs._mulrest ?? one).div(denom);
	bs.dA = dA;
	bs.dB = dB;
	bs._mulrest = ratio;
	return true;
}

// Hyperbolic × Exp products (these cycle infinitely under integration by parts)
export const hypExpRules: IntegrationRule[] = [
	// sinh×exp: sinh(A)·exp(B) → exp(B)/(b²-a²)·(b·sinh(A) - a·cosh(A))
	PatternRule('sinh×exp',
		symbolic.sinh(A).mul(symbolic.exp(B)),
		bs => symbolic.exp(bs.B).mul(symbolic.sinh(bs.A).mul(bs.dB).sub(symbolic.cosh(bs.A).mul(bs.dA))),
		checkHypExp
	),
	// cosh×exp: cosh(A)·exp(B) → exp(B)/(b²-a²)·(b·cosh(A) - a·sinh(A))
	PatternRule('cosh×exp',
		symbolic.cosh(A).mul(symbolic.exp(B)),
		bs => symbolic.exp(bs.B).mul(symbolic.cosh(bs.A).mul(bs.dB).sub(symbolic.sinh(bs.A).mul(bs.dA))),
		checkHypExp
	),
];

// Guard for trig products: checks dA and dB are constants, dA ≠ dB (else use squared rules)
function checkTrigProd(bs: Bindings): boolean {
	const v = varName(bs['wrt']);
	const dA = bs.A.derivative(v);
	const dB = bs.B.derivative(v);
	if (asNumeric(dA) instanceof symbolic || asNumeric(dB) instanceof symbolic)
		return false;
	const sum = dA.add(dB);
	const diff = dA.sub(dB);
	if (sum === zero || diff === zero)
		return false;
	const mulrest = bs._mulrest ?? one;
	bs.dA = dA;
	bs.dB = dB;
	bs._sum = sum;
	bs._diff = diff;
	bs._mulrest = mulrest;
	return true;
}

// Trig products with different arguments (use product-to-sum identities)
export const trigProdRules: IntegrationRule[] = [
	// sin(A)·cos(B) = ½[sin(A+B) + sin(A-B)] → -½[cos(A+B)/(dA+dB) + cos(A-B)/(dA-dB)]
	PatternRule('sin×cos',
		symbolic.sin(A).mul(symbolic.cos(B)),
		bs => symbolic.cos(bs.A.add(bs.B)).div(bs._sum).add(symbolic.cos(bs.A.sub(bs.B)).div(bs._diff)).div(-2),
		checkTrigProd
	),
	// sin(A)·sin(B) = ½[cos(A-B) - cos(A+B)] → ½[sin(A-B)/(dA-dB) - sin(A+B)/(dA+dB)]
	PatternRule('sin×sin',
		symbolic.sin(A).mul(symbolic.sin(B)),
		bs => symbolic.sin(bs.A.sub(bs.B)).div(bs._diff).sub(symbolic.sin(bs.A.add(bs.B)).div(bs._sum)).div(2),
		checkTrigProd
	),
	// cos(A)·cos(B) = ½[cos(A-B) + cos(A+B)] → ½[sin(A-B)/(dA-dB) + sin(A+B)/(dA+dB)]
	PatternRule('cos×cos',
		symbolic.cos(A).mul(symbolic.cos(B)),
		bs => symbolic.sin(bs.A.sub(bs.B)).div(bs._diff).add(symbolic.sin(bs.A.add(bs.B)).div(bs._sum)).div(2),
		checkTrigProd
	),
];
