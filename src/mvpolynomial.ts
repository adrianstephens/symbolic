import {scalar} from "../../maths/src/core";
import { toSubscript, toSuperscript } from "../../maths/src/string";

//-----------------------------------------------------------------------------
//	multivariate
//-----------------------------------------------------------------------------

// holds the power for each variable

export class Monomial extends Array<number> {
	constructor(...v: number[]) {
		if (v.length === 1) {
			super();
			this.push(v[0]);
		} else {
			super(...v);
		}
	}

	key(): string {
		return this.map((e, i) => `${i}:${e}`).join(',');
	}
	map<T>(f: (e: number, i: number, array: Monomial)=>T) {
		const result: T[] = [];
		for (const i in this)
			result.push(f(this[i], +i, this));
		return result;
	}
	degree(): number {
		return this.reduce((sum, e) => sum + e, 0);
	}

	without(v: number): Monomial {
		return new Monomial(...this.slice(0, v), ...this.slice(v + 1));
	}
 	divides(b: Monomial): boolean {
		return this.every((e, i) => e <= (b[i] ?? 0));
	}

	div(a: Monomial): Monomial | undefined {
		for (const i in a) {
			if (!this[i])
				return undefined;
		}

		const result = new Monomial;
		for (const i in this) {
			const diff = this[i] - (a[i] ?? 0);
			if (diff > 0)
				result[i] = diff;
		}
		return result;
	}

	mul(a: Monomial): Monomial {
		const result = new Monomial;
		for (const i in this)
			result[i] = this[i];

		for (const i in a)
			result[i] = (result[i] ?? 0) + a[i];
		return result;
	}

	static lcm(a: Monomial, b: Monomial): Monomial {
		const result = new Monomial;
		const n = Math.max(a.length, b.length);
		for (let i = 0; i < n; i++) {
			const maxExp = Math.max(a[i] ?? 0, b[i] ?? 0);
			if (maxExp > 0)
				result[i] = maxExp;
		}
		return result;
	}

	eq(a: Monomial): boolean {
		const n = Math.max(a.length, this.length);
		for (let i = 0; i < n; i++) {
			if ((a[i] ?? 0) !== (this[i] ?? 0))
				return false;
		}
		return true;
	}
	toString() {
		return this.map((e, i) => e === 0 ? '' : 'x' + toSubscript(i.toString()) + (e === 1 ? '' : toSuperscript(e.toString()))).join('');
	}
}

type Compare = (a: Monomial, b: Monomial) => number;

function compareGrevlex(a: Monomial, b: Monomial): number {
	const degA = a.degree();
	const degB = b.degree();
	
	if (degA !== degB)
		return degB - degA;
	
	// Reverse lex: compare from right to left
	const n = Math.max(a.length, b.length);
	for (let i = n - 1; i >= 0; i--) {
		const aExp = a[i] ?? 0;
		const bExp = b[i] ?? 0;
		if (aExp !== bExp)
			return aExp - bExp;
	}
	return 0;
}

export function lexOrder(a: Monomial, b: Monomial): number {
	const n = Math.max(a.length, b.length);

	for (let i = 0; i < n; i++) {
		const aExp = a[i] ?? 0;
		const bExp = b[i] ?? 0;
		if (aExp !== bExp)
			return bExp - aExp;
	}
	return 0;
}

export interface MVTerm<T> {
	coef:	T;
	mon:	Monomial;
}

export class MVPoly<T extends scalar<T>> {
	constructor(public terms: MVTerm<T>[] = []) {}

	dup() {
		return new MVPoly<T>(this.terms.slice());
	}
	isZero(): boolean {
		return this.terms.length === 0;
	}
	eq(b: MVPoly<T>) {
		return this.sub(b).isZero();
	}

	sort(ordering: Compare) {
		this.terms.sort((a, b) => ordering(a.mon, b.mon));
	}
	contains(v: number) {
		for (const t of this.terms)
			if (t.mon[v])
				return false;
		return true;
	}
	collect(v: number) {
		const result: MVPoly<T>[] = [];
		for (const t of this.terms) {
			if (t.mon[v])
				(result[t.mon[v]] ??= new MVPoly<T>).addTerm(t.mon.without(v), t.coef);
		}
		return result;
	}

	addTerm(mon: Monomial, coef: T) {
		for (const t of this.terms) {
			if (t.mon.eq(mon)) {
				t.coef = t.coef.add(coef);
				return;
			}
		}
		this.terms.push({mon, coef});
	}

	leadingTerm(ordering: Compare) {
		return this.terms.reduce((best, term) => ordering(best.mon, term.mon) > 0 ? term : best);
	}

	mulMono(m: Monomial) {
		const termMap = new Map<string, MVTerm<T>>();
		for (const t of this.terms) {
			const mon	= t.mon.mul(m);
			const key 	= mon.key();
			const existing = termMap.get(key);
			if (existing)
				existing.coef = existing.coef.add(t.coef);
			else
				termMap.set(key, {coef: t.coef, mon});
		}
		return new MVPoly(Array.from(termMap.values()));
	}

	add(b: MVPoly<T>) {
		const termMap = new Map<string, MVTerm<T>>(this.terms.map(t => [t.mon.key(), {...t}]));
		for (const t of b.terms) {
			const key 	= t.mon.key();
			const existing = termMap.get(key);
			if (existing)
				existing.coef = existing.coef.add(t.coef);
			else
				termMap.set(key, {coef: t.coef, mon: t.mon});
		}
		return new MVPoly(Array.from(termMap.values()));
	}
	sub(b: MVPoly<T>) {
		const termMap = new Map<string, MVTerm<T>>(this.terms.map(t => [t.mon.key(), {...t}]));
		for (const t of b.terms) {
			const key 	= t.mon.key();
			const existing = termMap.get(key);
			if (existing)
				existing.coef = existing.coef.sub(t.coef);
			else
				termMap.set(key, {coef: t.coef.neg(), mon: t.mon});
		}
		return new MVPoly(Array.from(termMap.values()).filter(t => t.coef.sign()));
	}

	divCoef(coef: T) {
		return new MVPoly(this.terms.map(t => ({ coef: t.coef.div(coef), mon: t.mon })));
	}
	mulCoef(coef: T) {
		return new MVPoly(this.terms.map(t => ({ coef: t.coef.mul(coef), mon: t.mon })));
	}

	mul(b: MVPoly<T>): MVPoly<T> {
		let result = new MVPoly<T>();
		for (const tb of b.terms)
			result = result.add(this.mulMono(tb.mon).mulCoef(tb.coef));
		return result;
	}

	// Multivariate polynomial division with remainder
	// Given f and divisors [g₁, g₂, ..., gₛ], compute:
	// f = q₁g₁ + q₂g₂ + ... + qₛgₛ + r
	divPoly(divisors: MVPoly<T>[], ordering: Compare): MVTerm<T>[][] {
		for (const d of divisors)
			d.sort(ordering);

		let dividend = this as MVPoly<T>;
		const quotients = divisors.map(() => [] as MVTerm<T>[]);
		const remainder: MVTerm<T>[] = [];
	
		while (!dividend.isZero()) {
			const	lt		= dividend.leadingTerm(ordering);
			let		divided = false;
	
			for (let i = 0; i < divisors.length; i++) {
				const ltg = divisors[i].terms[0];
				if (ltg.mon.divides(lt.mon)) {
					const c = lt.coef.div(ltg.coef);
					const m = lt.mon.div(ltg.mon)!;
					const subtrahend = divisors[i].mulMono(m).mulCoef(c);

					// Ensure the subtrahend actually cancels the current leading term.
					const ltSub = subtrahend.leadingTerm(ordering);
					if (!ltSub)
						continue;
					if (!ltSub.mon.eq(lt.mon))
						continue;

					// Accept subtraction
					quotients[i].push({coef: c, mon: m});
					dividend = dividend.sub(subtrahend);
					divided = true;
					break;
				}
			}
	
			if (!divided) {
				remainder.push(lt);
				dividend = dividend.sub(new MVPoly([lt]));
			}
		}
		this.terms = remainder;
		return quotients;
	}
	deriv(i: number) {
		const result = new MVPoly<T>();
		for (const t of this.terms) {
			const p = t.mon[i];
			if (p) {
				const mon = new Monomial(...t.mon);
				mon[i] = p - 1;
				result.addTerm(mon, t.coef.scale(p));
			}
		}
		return result;
	}

	toString() {
		return this.terms.map((t, j) => (j === 0 ? String(t.coef) : t.coef.sign() < 0 ? ` - ${t.coef.neg()}` : ` + ${t.coef}`) + String(t.mon)).join('') || '0';
	}
	
}

// S-polynomial: S(f, g) = (lcm/LT(f)) * f - (lcm/LT(g)) * g
function sPolynomial<T extends scalar<T>>(f: MVPoly<T>, g: MVPoly<T>, ordering: Compare): MVPoly<T> {
	const ltf = f.leadingTerm(ordering);
	const ltg = g.leadingTerm(ordering);
	
	if (!ltf || !ltg)

		return new MVPoly([]);
	
	const lcm = Monomial.lcm(ltf.mon, ltg.mon);
	
	// Scale to cancel leading terms: (mf/cf)*f - (mg/cg)*g
	return f.mulMono(lcm.div(ltf.mon)!).divCoef(ltg.coef)
		.sub(g.mulMono(lcm.div(ltg.mon)!).divCoef(ltg.coef));
}

//-----------------------------------------------------------------------------
// Groebner basis
//-----------------------------------------------------------------------------

export function groebnerBasis<T extends scalar<T>>(G: MVPoly<T>[], ordering = compareGrevlex): MVPoly<T>[] {
	// Expand all generators (GCD-factored muls will be unwrapped in leadingTerm)
	const pairs: [number, number][] = [];
	
	// Generate all pairs
	for (let i = 0; i < G.length; i++) {
		for (let j = i + 1; j < G.length; j++)
			pairs.push([i, j]);
	}
	
	while (pairs.length > 0) {
		const [i, j] = pairs.pop()!;
		const S = sPolynomial(G[i], G[j], ordering);
		S.divPoly(G, ordering);

		if (!S.isZero()) {
			// Skip if remainder already present in G (structural equality via id/string)
			if (G.some(g => g.eq(S)))
				continue;

			// Add new pairs with the new polynomial
			const newIndex = G.length;
			for (let k = 0; k < G.length; k++)
				pairs.push([k, newIndex]);

			G.push(S);
		}
	}

	// Reduce Groebner basis to minimal form

	// Make monic (leading coefficient = 1), but only if monomial is non-constant
	let result = G.map(g => {
		const lt = g.leadingTerm(ordering);
		// Don't make monic if the monomial is just 1 (constant polynomial in these variables)
		if (!lt || lt.mon.length === 0)
			return g;
		return g.divCoef(lt.coef);
	});
	
	// Remove redundant polynomials
	result = result.filter((f, i) => {
		const ltf = f.leadingTerm(ordering);
		if (!ltf)
			return false;
		return !result.some((g, j) => {
			if (i === j)
				return false;
			const ltg = g.leadingTerm(ordering);
			return ltg && ltg.mon.divides(ltf.mon);
		});
	});
	
	// Final inter-reduction: reduce each polynomial using the others
	// Only keep the reduction if it's actually simpler=
	result = result.map((f, i) => {
		const others = result.filter((_, j) => i !== j);
		if (others.length === 0)
			return f;
		const f2 = f.dup();
		f2.divPoly(others, ordering);
		// Only use remainder if it's non-zero (don't eliminate basis elements!)
		return f2.isZero() ? f : f2;
	});
	
	return result.filter(g => !g.isZero());
}

