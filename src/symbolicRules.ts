import Gen from '@isopodlabs/maths/gen';
import rational from '@isopodlabs/maths/rational';
import { symbolic, MatchOptions, Bindings, term, factor, mulFactors, addTerms, factorAsSymbolic } from './symbolic';
import { MVPoly, Monomial } from '@isopodlabs/maths/mvpolynomial';

export type Scorer = (n: symbolic, best?: number) => number;

export type Rule = {
	name:		string;
	match:		(node: symbolic, opts?: MatchOptions) => Bindings | null;
	replace:	(bs: Bindings) => symbolic;
	guard?:		(bs: Bindings, context?: any) => boolean;
};


export function scoreFactory(depthPenalty = 0.2, mulW = 0.5, addW = 1, constW = 0.1) {
    return (n: symbolic, bestScore?: number) => {
        let total = 0;
		let depth = 1;
		bestScore ??= Infinity;

        n.visit({
			noRemake: true,
            pre: (x: symbolic) => {
				if (total > bestScore)
					return undefined;
                ++depth;
                return x;
            },
            post: (x: symbolic) => {
                --depth;
                const w = x.is('mul') ? (x.factors.length - 1) * mulW
               		: x.is('add') ? (x.terms.length - 1) * addW
               		: x.is('const') ? constW
               		: 0.5;
                total += w * (1 + depthPenalty * depth);
                return x;
            }
        });

        return total;
    };
}

export function applyRules(node: symbolic, rules: Rule[], opts?: { allowRecursive?: boolean }): symbolic {
	const cache		= new Map<string, symbolic>();
	const allowRecursive = opts?.allowRecursive !== false;

	const visitor = {
		post: (node: symbolic) => {
			const cached = cache.get(node.id);
			if (cached)
				return cached;

			for (const r of rules) {
				const bs = r.match(node);
				if (bs && (!r.guard || r.guard(bs))) {
					const replaced = r.replace(bs);
					// When recursive simplification is disabled we avoid
					// visiting the replaced subtree with the same visitor.
					const simplified = allowRecursive ? replaced.visit(visitor) : replaced;
					cache.set(node.id, simplified);
					return simplified;
				}
			}
			cache.set(node.id, node);
			return node;
		}
	};

	return node.visit(visitor);
}

//-----------------------------------------------------------------------------
// factoring
//-----------------------------------------------------------------------------

interface factorSet extends factor {
	terms: Set<term>;
}

interface factorCandidate {
	terms:	Set<term>;
	mul:	symbolic;
	remove: (t: term) => term;
}

// collect factor counts and min powers across terms
function getFactors(terms: readonly Readonly<term>[]): factorSet[] {
	const factors: Record<string, factorSet> = {};

	function addFactor(item: symbolic, pow: rational, t: term) {
		const info = factors[item.id] ??= { item: item, pow: pow, terms: new Set<term>() };
		info.terms.add(t);
		info.pow = Gen.min(info.pow, pow);
	}
	for (const t of terms) {
		if (t.item.is('mul')) {
			for (const f of t.item.factors)
				addFactor(f.item, f.pow, t);
		} else {
			addFactor(t.item, rational(1), t);
		}
	}
	return Object.values(factors);
}

function makeCandidate1(terms: Set<term>, a: factor): factorCandidate {
	return {
		terms,
		get mul() { return factorAsSymbolic(a); },
		remove: (t: term) => {
			if (t.item.is('mul')) {
				const newFactors = t.item.factors.map(f =>
					f.item === a.item ? factor(f.item, f.pow.sub(a.pow))
					: f
				).filter(f => !f.pow.is0());
				return term(mulFactors(t.item.num, ...newFactors), t.coef);
			}
			return t.item === a.item ? term(one, t.coef) : t;
		}

	};
}
function makeCandidate2(terms: Set<term>, a: factor, b: factor): factorCandidate {
	return {
		terms,
		get mul() { return factorAsSymbolic(a).mul(factorAsSymbolic(b)); },
		remove: (t: term) => {
			if (t.item.is('mul')) {
				const newFactors = t.item.factors.map(f =>
					f.item === a.item ? factor(f.item, f.pow.sub(a.pow))
				:   f.item === b.item ? factor(f.item, f.pow.sub(b.pow))
				:   f
				).filter(f => !f.pow.is0());
				return term(mulFactors(t.item.num, ...newFactors), t.coef);
			}
			return t;
		}
	};
}
function getFactorCandidates(terms: readonly Readonly<term>[]): factorCandidate[] {
	const singles = getFactors(terms).filter(i => i.terms.size > 1);

	singles.sort((a, b) => b.terms.size - a.terms.size);
	singles.length = Math.min(singles.length, 16);

	const pairs: {terms: Set<term>, a: factor, b: factor}[] = [];
	for (const a of singles) {
		for (const b of singles) {
			if (a === b)
				break;
			const terms = a.terms.intersection(b.terms);
			if (terms.size > 1)
				pairs.push({terms, a, b});
		}
	}
	pairs.sort((a, b) => b.terms.size - a.terms.size);
	pairs.length = Math.min(pairs.length, 16);

	return [
		...singles.map(i => makeCandidate1(i.terms, i)),
		...pairs.map(i => makeCandidate2(i.terms, i.a, i.b))
	];
}

function applyFactorCandidate(terms: Set<term>, f: factorCandidate) {
	const terms2 = f.terms.intersection(terms);
	for (const v of terms2)
		terms.delete(v);
	return addTerms(rational(0), ...Array.from(terms2).map(t => f.remove(t))).mul(f.mul);
}


function commonFactors(terms: readonly Readonly<term>[], scorer: (sym: symbolic) => number): readonly term[] {
	const singles = getFactors(terms).filter(i => i.terms.size > 1);

	singles.sort((a, b) => b.terms.size - a.terms.size);
	singles.length = Math.min(singles.length, 16);

	const pairs: {terms: Set<term>, a: factor, b: factor}[] = [];
	for (const a of singles) {
		for (const b of singles) {
			if (a === b)
				break;
			const terms = a.terms.intersection(b.terms);
			if (terms.size > 1)
				pairs.push({terms, a, b});
		}
	}
	pairs.sort((a, b) => b.terms.size - a.terms.size);
	pairs.length = Math.min(pairs.length, 16);

	const candidates = new Set(getFactorCandidates(terms));

	// prefer candidates that cover more terms and that simplify their inner sum
	const scoreCandidate = (terms: Set<term>, inner: symbolic) => terms.size * 10 - scorer(inner);

	const factored: term[] = [];
	let remaining = new Set<term>(terms);

	while (remaining.size > 1) {
		// find best candidate
		type ProcessedCandidate = { candidate: factorCandidate; terms: Set<term>; inner: symbolic; score: number };
		let best: ProcessedCandidate | undefined;

		for (const candidate of candidates) {
			const terms = candidate.terms.intersection(remaining);
			if (terms.size > 1)  {
				const inner = addTerms(rational(0), ...Array.from(terms).map(t => candidate.remove(t)));
				const score = scoreCandidate(terms, inner);
				if (!best || score > best.score)
					best = { candidate, terms, inner, score };
			}
		}

		if (!best)
			break;

		factored.push(term(best.inner.mul(best.candidate.mul)));
		remaining = remaining.difference(best.terms);
		candidates.delete(best.candidate);
	}

	return [...factored, ...remaining];
}


export function factored(node: symbolic) {
	if (node.is('mul'))
		return node.visit({
			post: (x: symbolic): symbolic => x.is('add') ? factored(x) : x,
		});
	if (!node.is('add'))
		return node;
	const scorer = scoreFactory();
	const terms = commonFactors(node.terms, scorer);
	return addTerms(node.num, ...terms);
}

//-----------------------------------------------------------------------------
// multivariate polynomials
//-----------------------------------------------------------------------------

export function toMVPoly(expr: symbolic, variables: readonly symbolic[]): MVPoly<symbolic> {
	expr = expr.expand();
	
	let scale = rational(1);

	if (expr.is('mul') && expr.factors.length === 1 && expr.factors[0].pow.is1() && expr.factors[0].item.is('add')) {
		scale = expr.num;
		expr = expr.factors[0].item;
	}

	const terms = expr.is('add') ? expr.terms as term[] : [term(expr)];
	const poly = new MVPoly<symbolic>();

	for (const i of terms) {
		const mon = new Monomial;

		if (i.item.is('mul')) {
			const coefFactors: symbolic[] = [symbolic.from(i.coef)];

			for (const factor of i.item.factors) {
				const idx = variables.indexOf(factor.item);
				if (idx >= 0) {
					mon[idx] = Number(factor.pow);
				} else {
					coefFactors.push(factor.item.npow(factor.pow));
				}
			}

			poly.addTerm(mon, coefFactors.length === 0 ? symbolic.from(1) : coefFactors.reduce((a, b) => a.mul(b)));
		} else {
			const idx = variables.indexOf(expr);
			if (idx >= 0) {
				mon[idx] = 1;
				poly.addTerm(mon, symbolic.from(i.coef));
			} else {
				poly.addTerm(mon, expr.scale(i.coef));
			}
		}
	}
	if (scale !== rational(1))
		return poly.mulCoef(symbolic.from(scale));
	return poly;
}

export function fromMVPoly(poly: MVPoly<symbolic>, variables: readonly symbolic[]): symbolic {
	return poly.terms.reduce((acc: symbolic, term) => 
		acc.add(mulFactors(rational(1), factor(term.coef), ...term.mon.map((exp, i) => 
			factor(variables[i], rational(exp))
		))),
		symbolic.one
	);
}

//-----------------------------------------------------------------------------
// transformation rules
//-----------------------------------------------------------------------------

function replaceRest(bs: Bindings, node: symbolic): symbolic {
	const addrest = bs['_addrest'];
	if (addrest)
		node = node.add(addrest);
	const mulrest = bs['_mulrest'];
	if (mulrest)
		node = node.mul(mulrest);
	return node;
}

function PatternRule(name: string, pattern: symbolic, replace: (bs: Bindings) => symbolic, guard?: (bs: Bindings, context: any) => boolean): Rule {
	return {
		name,
		match: (node, opts) => pattern.match(node, {}, opts),
		replace: bs => replaceRest(bs, replace(bs)),
		guard,
	};
}

function FactorRule(name: string, index: number): Rule {
	return {
		name,
		match(node: symbolic) {
			if (node.is('add')) {

				const factors	= getFactorCandidates(node.terms).sort((a, b) => b.terms.size - a.terms.size);
				if (factors.length > index) {
					const factor	= factors[index];
					const terms		= new Set(node.terms);
					const inner		= applyFactorCandidate(terms, factor);
					const result	= addTerms(node.num, ...Array.from(terms), term(inner));
					return { result };
				}
				/*
				const factors	= getFactors(node.terms).filter(i => i.terms.size > 1).sort((a, b) => b.terms.size - a.terms.size);
				if (factors.length > index) {
					const factor	= makeCandidate1(factors[index].terms, factors[index]);
					const terms		= new Set(node.terms);
					const inner		= applyFactorCandidate(terms, factor);
					const result	= addTerms(node.num, ...Array.from(terms), term(inner));

					//const result	= removeFactor(node, factor);
					return { result };
				}
					*/
			}
			return null;
		},
		replace(bs: Bindings) {
			return bs.result;
		}
	};
}

const A		= symbolic.bind('A');
const B		= symbolic.bind('B');
const C		= symbolic.bind('C');
const U		= symbolic.bind('U');
const V		= symbolic.bind('V');

const one	= symbolic.one;
const zero	= symbolic.zero;
const pi	= symbolic.pi;

export const generalRules: Rule[] = [
	{
		name: 'mul-distribute',
		match(node: symbolic) {
			if (node.is('mul')) {
				if (node.factors.length == 1) {
					const npow = Number(node.factors[0].pow);
					if (npow < 2 || !Number.isInteger(npow))
						return null;
				}
				// Heuristic: avoid expanding very large expressions
				if (String(node).length > 400)
					return null;

				const result = node.expand({ depth: 1, maxPow: 4, maxParts: 50 });
				if (result !== node && String(result).length < 800)
					return {result};
			}
			return null;
		},
		replace(bs: Bindings): symbolic {
			return bs.result;
		}
	},

	// narrow fraction rules: same denominator and one-denominator-is-product-of-other
	// rewrite A/U - V  ->  (A - U*V) / U
	// rewrite V - A/U  ->  (U*V - A) / U
	PatternRule('sub-over-denom',
		A.div(U).sub(V),
		//V.sub(A.div(U)),
		bs => bs.A.sub(bs.U.mul(bs.V)).div(bs.U)
		//bs => bs.U.mul(bs.V).sub(bs.A).div(bs.U)
	),

	// rewrite A/U - B/V  ->  (A*V - B*U) / (U*V)
	PatternRule('sub-over-denoms 2',
		A.div(U).sub(B.div(V)),
		bs => bs.A.mul(bs.V).sub(bs.B.mul(bs.U)).div(bs.U.mul(bs.V))
	),

	{
		name: 'collect-like-terms',
		match(node: symbolic) {
			if (node.is('add') && node.terms.length > 1 && node.terms.some(t => t.item.is('mul'))) {
				const scorer = scoreFactory();
				const terms = commonFactors(node.terms, scorer);
				const result = addTerms(node.num, ...terms);
				if (result !== node)
					return { result };
			}
			return null;
		},
		replace(bs: Bindings) {
			return bs.result;
		}
	},

	//FactorRule('factor-common-terms', 0),

	//FactorRule('factor-common-terms 2', 1),
	//FactorRule('factor-common-terms 3', 2),
	//FactorRule('factor-common-terms 4', 3),
	//FactorRule('factor-common-terms 5', 4),

	// quadratics:

	// Perfect square trinomial: A^2 + 2AB + B^2 -> (A+B)^2
	PatternRule('perfect-square-sum',
		A.npow(2).add(A.mul(B).scale(2)).add(B.npow(2)),
		bs => bs.A.add(bs.B).npow(2)
	),
	// Perfect square trinomial: A^2 - 2AB + B^2 -> (A-B)^2
	PatternRule('perfect-square-diff',
		A.npow(2).sub(A.mul(B).scale(2)).add(B.npow(2)),
		bs => bs.A.sub(bs.B).npow(2)
	),
	// Difference of squares: a² - b² → (a-b)(a+b)
	PatternRule('factor-diff-squares',
		A.npow(2).sub(B.npow(2)),
		bs => bs.A.sub(bs.B).mul(bs.A.add(bs.B))
	),
	
	// cubics:

	// Perfect square trinomial (3 vars): A^2 + B^2 + C^2 + 2AB + 2AC + 2BC -> (A+B+C)^2
	PatternRule('perfect-square-sum-3',
		A.npow(2).add(B.npow(2)).add(C.npow(2)).add(A.mul(B).scale(2)).add(A.mul(C).scale(2)).add(B.mul(C).scale(2)),
		bs => bs.A.add(bs.B).add(bs.C).npow(2)
	),
	// Perfect square trinomial (3 vars): A^2 + B^2 + C^2 - 2AB - 2AC - 2BC -> (A-B-C)^2
	PatternRule('perfect-square-diff-3',
		A.npow(2).add(B.npow(2)).add(C.npow(2)).sub(A.mul(B).scale(2)).sub(A.mul(C).scale(2)).sub(B.mul(C).scale(2)),
		bs => bs.A.sub(bs.B).sub(bs.C).npow(2)
	),

	// Symmetric cubic: (A+B+C)^3 - 3(A+B+C)(AB+AC+BC) + 3ABC -> A^3+B^3+C^3
	//PatternRule('symmetric-cubic-expand',
	//	A.add(B).add(C).npow(3).sub(A.add(B).add(C).mul(A.mul(B).add(A.mul(C)).add(B.mul(C))).scale(3)).add(A.mul(B).mul(C).scale(3)),
	//	bs => bs.A.npow(3).add(bs.B.npow(3)).add(bs.C.npow(3))
	//),
	// Symmetric cubic contraction: A^3+B^3+C^3 -> (A+B+C)^3 - 3(A+B+C)(AB+AC+BC) + 3ABC
	PatternRule('symmetric-cubic-contract',
		A.npow(3).add(B.npow(3)).add(C.npow(3)),
		bs => bs.A.add(bs.B).add(bs.C).npow(3).sub(bs.A.add(bs.B).add(bs.C).mul(bs.A.mul(bs.B).add(bs.A.mul(bs.C)).add(bs.B.mul(bs.C))).scale(3)).add(bs.A.mul(bs.B).mul(bs.C).scale(3))
	),
	// AB+AC+BC contraction: (A+B)C + AB -> AB + AC + BC
	//PatternRule('abacbc-contract',
	//	A.add(B).mul(C).add(A.mul(B)),
	//	bs => bs.A.mul(bs.B).add(bs.A.mul(bs.C)).add(bs.B.mul(bs.C))
	//),
	// AB+AC+BC expansion: AB + AC + BC -> (A+B)C + AB
	PatternRule('abacbc-expand',
		A.mul(B).add(A.mul(C)).add(B.mul(C)),
		bs => bs.A.add(bs.B).mul(bs.C).add(bs.A.mul(bs.B))
	),
	
	// Cube contraction: A^3 + 3A^2B + 3AB^2 + B^3 -> (A+B)^3
	PatternRule('contract-cube-sum',
		A.npow(3).add(A.npow(2).mul(B).scale(3)).add(A.mul(B.npow(2)).scale(3)).add(B.npow(3)),
		bs => bs.A.add(bs.B).npow(3)
	),
	// Cube contraction: A^3 - 3A^2B + 3AB^2 - B^3 -> (A-B)^3
	PatternRule('contract-cube-diff',
		A.npow(3).sub(A.npow(2).mul(B).scale(3)).add(A.mul(B.npow(2)).scale(3)).sub(B.npow(3)),
		bs => bs.A.sub(bs.B).npow(3)
	),

	(() => {
		// Bridge the explicit Cardano closed form (built from the symmetric sums of A,B,C)
		// back to one of the roots. This is intentionally specific so we can collapse
		// the radical expression you get from Cardano when the cubic already factors
		// as (x-A)(x-B)(x-C).
		const S1 = A.add(B).add(C);
		const S2 = A.mul(B).add(A.mul(C)).add(B.mul(C));
		const P = S2.sub(S1.npow(2).scale(1/3));
		const Q = S1.npow(3).scale(-2/27).add(S1.mul(S2).scale(1/3)).sub(A.mul(B).mul(C));
		const Delta = Q.scale(0.5).npow(2).add(P.scale(1/3).npow(3));
		const cardano = /*factored*/(S1.scale(1/3)
			.add(Q.scale(-0.5).add(Delta.rpow(1, 2)).rpow(1, 3))
			.add(Q.scale(-0.5).sub(Delta.rpow(1, 2)).rpow(1, 3))
		);
		return PatternRule('cardano-abc-root-A', cardano, bs => bs.A,
			bs => true
		);
	})(),
];

export const trigRules: Rule[] = [
	// sin(A+B) = sin(A)cos(B) + cos(A)sin(B)
	PatternRule('sin-sum',
		symbolic.sin(A.add(B)),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	// sin(A-B) = sin(A)cos(B) - cos(A)sin(B)
	PatternRule('sin-diff',
		symbolic.sin(A.sub(B)),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	// sin(x/2) = sqrt((1-cos(x))/2)
	PatternRule('sin-half-angle',
		symbolic.sin(A.scale(0.5)),
		bs => symbolic.sqrt(one.sub(symbolic.cos(bs.A)).scale(0.5))
	),
	// sin(2x) = 2 * sin(x) * cos(x)
	PatternRule('sin-double-angle',
		symbolic.sin(A.scale(2)),
		bs => symbolic.from(2).mul(symbolic.sin(bs.A).mul(symbolic.cos(bs.A)))
	),


	// cos(A+B) = cos(A)cos(B) - sin(A)sin(B)
	PatternRule('cos-sum',
		symbolic.cos(A.add(B)),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	// cos(A-B) = cos(A)cos(B) + sin(A)sin(B)
	PatternRule('cos-diff',
		symbolic.cos(A.sub(B)),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	// cos(x/2) = sqrt((1+cos(x))/2)
	PatternRule('cos-half-angle',
		symbolic.cos(A.scale(0.5)),
		bs => symbolic.sqrt(one.add(symbolic.cos(bs.A)).scale(0.5))
	),
	// cos(2x) = cos^2(x) - sin^2(x)
	PatternRule('cos-double-angle',
		symbolic.cos(A.scale(2)),
		bs => symbolic.cos(bs.A).npow(2).sub(symbolic.sin(bs.A).npow(2))
	),


	// tan(A+B) = (tan(A) + tan(B)) / (1 - tan(A)tan(B))
	PatternRule('tan-sum',
		symbolic.tan(A.add(B)),
		bs => symbolic.tan(bs.A).add(symbolic.tan(bs.B)).div(one.sub(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	// tan(A-B) = (tan(A) - tan(B)) / (1 + tan(A)tan(B))
	PatternRule('tan-diff',
		symbolic.tan(A.sub(B)),
		bs => symbolic.tan(bs.A).sub(symbolic.tan(bs.B)).div(one.add(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	// tan(x/2) = sin(x) / (1 + cos(x))
	PatternRule('tan-half-angle',
		symbolic.tan(A.scale(0.5)),
		bs => symbolic.sin(bs.A).scale(0.5).div(symbolic.cos(bs.A).scale(0.5))
	),

	// Pythagorean identities
	PatternRule('sin-cos-pythag',
		symbolic.sin(A).npow(2).add(symbolic.cos(A).npow(2)),
		_ => one
	),
	PatternRule('tan-secant-pythag',
		symbolic.tan(A).npow(2).add(one),
		bs => symbolic.cos(bs.A).npow(-2)
	),
	PatternRule('cot-cosecant-pythag',
		one.add(symbolic.tan(A).npow(-2)),
		bs => symbolic.sin(bs.A).npow(-2)
	),
	/*
	PatternRule('sin-cos-product',
		symbolic.sin(A).mul(symbolic.cos(B)),
		bs => symbolic.sin(bs.A.add(bs.B)).mul(symbolic.cos(bs.A.sub(bs.B))).scale(0.5)
	),
	*/
];

export const atan2Rules: Rule[] = [
	// atan2 basic properties
	PatternRule('atan2-to-atan',
		symbolic.atan2(B, A),
		bs => symbolic.atan(bs.B.div(bs.A)),
	),

	// atan2 symmetry
	PatternRule('atan2-negate-both',
		symbolic.atan2(B.neg(), A.neg()),
		bs => symbolic.atan2(bs.B, bs.A)
	),

	// atan2 scale invariance
	PatternRule('atan2-scale-invariance',
		symbolic.atan2(B.mul(C), A.mul(C)),
		bs => symbolic.atan2(bs.B, bs.A),
	),

	// atan2 of zero
	PatternRule('atan2-zero-y',
		symbolic.atan2(zero, A),
		_ => zero
	),

	// atan2 special angles
	PatternRule('atan2-x-x',
		symbolic.atan2(A, A),
		_ => pi.scale(1/4)
	),
	PatternRule('atan2-x-neg-x',
		symbolic.atan2(A, A.neg()),
		_ => pi.scale(3/4)
	),
];

export const invTrigRules: Rule[] = [
	// sin(A)cos(B) + cos(A)sin(B) -> sin(A+B)
	PatternRule('sin-sum-compress',
		symbolic.sin(A).mul(symbolic.cos(B)).add(symbolic.cos(A).mul(symbolic.sin(B))),
		bs => symbolic.sin(bs.A.add(bs.B)),
	),
	// sin(x) * cos(x) -> 1/2 * sin(2x)
	PatternRule('double-angle-sin-compress',
		symbolic.sin(A).mul(symbolic.cos(A)),
		bs => symbolic.sin(bs.A.scale(2)).scale(0.5)
	),

	// cos^2(x) - sin^2(x) -> cos(2x)
	PatternRule('double-angle-cos-compress',
		symbolic.cos(A).npow(2).sub(symbolic.sin(A).npow(2)),
		bs => symbolic.cos(bs.A.scale(2))
	),

	/*
	PatternRule( 'sin-cos-product',
		symbolic.sin(A.add(B)).mul(symbolic.cos(A.sub(B))),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B))
	),
	*/
];
