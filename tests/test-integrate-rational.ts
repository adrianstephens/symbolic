/* eslint-disable @typescript-eslint/no-unused-vars */
import { test, assert } from './test';
import {canop} from '@isopodlabs/maths/core';
import { hermiteReduction, rothsteinTrager, integrateRational } from '../src/rationalExpr';
import gen from '@isopodlabs/maths/gen';
import { Polynomial, partialFractions, Factor, Extension, squareFreeFactorization, resultant, factorOverK_Q, factorSquareFreeOverK_Q, PolyTypes, factorType } from '@isopodlabs/maths/polynomial';
import { symbolic } from '../src/symbolic';
import rational, { rationalT } from '@isopodlabs/maths/rational';

type FactorExt<T>	= Polynomial<Polynomial<T>> | Extension<T>;

const zero = Polynomial([0]);
const one = Polynomial([1]);
const t = Polynomial([0, 1]);
const x = symbolic.variable('x');

// Expand extension descriptors numerically via resultant elimination in ζ and
// verify the full product (expanded factors) equals S2 by checking at several t-values.
function evalPolyPolyAtT(p: Polynomial<Polynomial<rational>>, tval: number): Polynomial<rational> {
	const rt = rational.from(tval);
	return p.map(c => c.evaluate(rt));
}

function evalPartAtT(part: FactorExt<rational>, tval: number): Polynomial<rational> {
	if (!('alpha' in part))
		return evalPolyPolyAtT(part, tval);

	const pZeta = Polynomial(part.poly.c.map(ci => Polynomial([ci])));
	const qZeta = Polynomial(part.alpha.c.map((c, k) => {
		const ak = c.evaluate(rational(tval)).neg();
		return k === 0 ? Polynomial([ak, rational(1)]) :  Polynomial([ak]);
	}));
	const res = resultant(pZeta, qZeta);
	if (!res)
		throw new Error('Failed to compute resultant for extension factor');
	return res;
}


function factorString<T>(f: Factor<T>) {
	return `${f.factor} (x${f.multiplicity})`
}

function factorExtString<T>(f: FactorExt<T>) {
	return 'alpha' in f
		? `alpha: ${f.alpha}, poly: ${f.poly}`
		: f.toString();
}
function toSym(i: number) {
	return symbolic.from(i);
}

interface FactorTerms<T> {
	factor:		T;
	numerators:	T[];
}

function showPF<T extends PolyTypes>(polyPart: Polynomial<T>, factors: FactorTerms<Polynomial<T>>[]) {
	console.log(`poly: ${polyPart}\nfactors:\n${factors.map(f => `${f.factor} terms:${f.numerators.map((t, i) => String(rationalT(t, f.factor.ipow(i + 1)))).join(', ')}`).join('\n')}`);
}

function verifyPF<T extends PolyTypes & canop<'valueOf'>>(N: Polynomial<T>, D: Polynomial<T>, polyPart: Polynomial<T>, factors: FactorTerms<Polynomial<T>>[], testVals: T[]) {
	for (const xval of testVals) {
		const orig = gen.div(N.evaluate(xval), D.evaluate(xval));

		let sum = polyPart.evaluate(xval);
		for (const {factor, numerators} of factors) {
			let fpow = factor;
			for (const t of numerators) {
				sum = gen.add(sum, gen.div(t.evaluate(xval), fpow.evaluate(xval)));
				fpow = fpow.mul(factor);
			}
		}
		assert(gen.approx(orig, sum), `partialFractions: value mismatch at x=${xval}`);
	}
}

function showHermite<T>(factor: Polynomial<T>, derivs: Polynomial<T>[], remainder: Polynomial<T>) {
	console.log(`derivs: ${derivs.map((d, i) => rationalT(d, factor.ipow(i + 1))).join(', ')}\nremainder: ${rationalT(remainder, factor)}\n`);
}

function recombineHermite<T extends PolyTypes>(
	polyPart:	Polynomial<T>,
	derivs:		rationalT<Polynomial<T>>[],
	remainders:	rationalT<Polynomial<T>>[],
) {
	let sum = polyPart && polyPart.degree() >= 0 ? rationalT(polyPart) : undefined;
	for (const t of derivs) {
		if (t.num) {
			const d = derivative(t);
			sum = sum?.add(d) ?? d;
		}
	}
	for (const t of remainders)
		sum = sum?.add(t) ?? t;
	return sum;
}

function verifyHermite<T extends PolyTypes & canop<'valueOf'>>(N: Polynomial<T>, D: Polynomial<T>,
	polyPart:	Polynomial<T>,
	derivs:		rationalT<Polynomial<T>>[],
	remainders:	rationalT<Polynomial<T>>[],
	testVals:	T[])
{
	for (const x of testVals) {
		const orig = gen.div(N.evaluate(x), D.evaluate(x));

		let sum = polyPart?.evaluate(x);
		for (const t of derivs) {
			if (t.num) {
				const d = derivative(t);
				sum = gen.add(gen.div(d.num.evaluate(x), d.den.evaluate(x)), sum);
			}
		}
		for (const t of remainders) {
			if (t.num)
				sum = gen.add(gen.div(t.num.evaluate(x), t.den.evaluate(x)), sum);
		}

		assert(gen.approx(orig, sum), `hermiteReduce: value mismatch at x=${x}`);
	}
}

function derivative<T>(r: rationalT<Polynomial<T>>) {
	return rationalT<Polynomial<T>>(r.den.mul(r.num.deriv()).sub(r.num.mul(r.den.deriv())), r.den.mul(r.den));
}

test('hermiteReduce', () => {
	const d0 = Polynomial<number>([1, 0, 1]), d1 = Polynomial<number>([-2, 1]);

	test(
		Polynomial([1]),
		d0.ipow(2),
		[0, -3, -1, 1, 2, 3]
	);

	// Example: 1/(x^3 + x) = 1/(x(x^2+1))
	test(
		Polynomial([1]),
		Polynomial([0, 1, 0, 1]), // x^3 + x
		[1, 2, -1]
	);

	// Example: 1/(x^2 + 1)
	test(
		Polynomial([1]),
		d0, // x^2 + 1
		[1, 2, -1]
	);


	// Much more complicated case: (3x^7 - 2x^5 + 7x^3 - 4x + 5) / (x^8 - 2x^6 + x^4 - x^2 + 1)
	test(
		Polynomial([5, -4, 0, 7, 0, -2, 0, 3]),
		Polynomial([1, 0, -1, 0, 1, 0, -2, 0, 1]),
		[0, 2, -2]
	);

	{
		//const A = Polynomial([0]), B = Polynomial([0, 1/2]), C = Polynomial([1/2])
		const A = Polynomial([0,3/8]), B = Polynomial([0, 1/4]), C = Polynomial([3/8])
	
		const polyPart	= Polynomial([0]);
		const derivs:		rationalT<Polynomial<number>>[]	= [rationalT(A, d0.ipow(1)), rationalT(B, d0.ipow(2))];
		const remainders:	rationalT<Polynomial<number>>[] = [rationalT(C, d0)];

		verifyHermite(
			Polynomial([symbolic.one]),
			d0.map(i => symbolic.from(i)).ipow(3),
			polyPart.map(toSym),
			derivs.map(p => rationalT(p.num.map(toSym), p.den.map(toSym))),
			remainders.map(p => rationalT(p.num.map(toSym), p.den.map(toSym))),
			[0, -3, -1, 1, 2, 3].map(i=>symbolic.from(i))
		);

		verifyHermite(
			Polynomial([1]),
			d0.ipow(3),
			polyPart,
			[rationalT(A, d0.ipow(1)), rationalT(B, d0.ipow(2))],
			[rationalT(C, d0)],
			[0, -3, -1, 1, 2, 3]
		);
	}

	test(
		Polynomial([1]),
		d0.ipow(3),
		[0, -3, -1, 1, 2, 3]
	);

	test(
		Polynomial([-8, 4, -7, 5, -1, 3, 2]),
		d0.ipow(3).mul(d1.ipow(2)),
		[0, -3, -1, 0, 1, 3]
	);


	function testHermite<T extends factorType & canop<'valueOf'>>(N: Polynomial<T>, D: Polynomial<T>, testVals?: T[]) {
		console.log(`hermiteReduce: ${rationalT(N, D)}`);

		const { polyPart, factors } = partialFractions(N, D);
		showPF(polyPart, factors)

		if (testVals)
			verifyPF(N, D, polyPart, factors, testVals);

		const allDerivs:	rationalT<Polynomial<T>>[] = [];
		const allRemainders:rationalT<Polynomial<T>>[] = [];
		for (const { factor, numerators } of factors) {
			const {derivs, remainder} = hermiteReduction(factor, numerators);
			showHermite(factor, derivs, remainder);
			allDerivs.push(...derivs.map((d, i) => rationalT(d, factor.ipow(i + 1))));
			allRemainders.push(rationalT(remainder, factor));
		}

		const sum = recombineHermite(polyPart, allDerivs, allRemainders);
		console.log(`recombined: ${sum}`);

		if (testVals) {
			verifyHermite(N, D, polyPart, allDerivs, allRemainders, testVals);
			console.log('verified');
		}
	}

	function test(N: Polynomial<number>, D: Polynomial<number>, testVals?: number[]) {
		testHermite(N.map(toSym), D.map(toSym), testVals?.map(toSym));
		testHermite(N, D, testVals);
	}
});

test('test0', ()=> {
	const f = Polynomial([symbolic.from(1), symbolic.from(-2), symbolic.from(1)]);
	const g = Polynomial([symbolic.from(-2), symbolic.from(2)]);
	const f1 = f.dup();
	f1.pseudoDivmod(g);
	console.log(String(f1));

	const sf = squareFreeFactorization(f);
	console.log(sf);
});

test('test deriv', ()=> {
	const a = symbolic.sym('a');
	const b = symbolic.sym('b');
	const c = symbolic.sym('c');

// 4) quadratic irreducible: 1/(x^2+1) -> arctan
/*	test(
		Polynomial([symbolic.from(1)]),
		Polynomial([symbolic.from(1), symbolic.from(0), symbolic.from(1)]), // x^2+1
		symbolic.atan(x)
	);*/

	// 6) symbolic-coefficient smoke test
	const C			= a.div(symbolic.from(2));
	const sqrtc		= c.sqrt();
	const atanTerm	= symbolic.atan(x.div(sqrtc)).mul(b.div(sqrtc));

	// integrand: (a*x + b) / (x^2 + c) => log(x^2+c) * a/2 + atan(x/sqrt(c)) * b/sqrt(c)
	test(
		Polynomial([b, a]),
		Polynomial([c, symbolic.zero, symbolic.one]),
		symbolic.log(x.ipow(2).add(c)).mul(C).add(atanTerm)
	);

	// 4) quadratic irreducible: 1/(x^2+1) -> arctan
	test(
		Polynomial([symbolic.from(1)]),
		Polynomial([symbolic.from(1), symbolic.from(0), symbolic.from(1)]), // x^2+1
		symbolic.atan(x)
	);


	test(
		Polynomial([symbolic.one]),
		Polynomial([symbolic.from(-1), symbolic.zero, a])
	);

	// 1) derivative-multiple: 3*x^2 / (x^3 + 1) -> ln(x^3+1)
	test(
		Polynomial([symbolic.from(0), symbolic.from(0), symbolic.from(3)]),
		Polynomial([symbolic.from(1), symbolic.from(0), symbolic.from(0), symbolic.from(1)]),
		symbolic.log(x.ipow(3).add(symbolic.from(1)))
	);

	// 2) simple linear factor: 1/(x-2) -> ln(x-2)
	test(
		Polynomial([symbolic.from(1)]),
		Polynomial([symbolic.from(-2), symbolic.from(1)]), // x-2
		symbolic.log(x.add(symbolic.from(-2)))
	);

// 3) repeated linear factor: 1/(x-1)^2 -> -1/(x-1)
	test(
		Polynomial([symbolic.from(1)]),
		Polynomial([symbolic.from(1), symbolic.from(-2), symbolic.from(1)]), // (x-1)^2
		symbolic.from(-1).mul(x.add(symbolic.from(-1)).ipow(-1))
	);

// 4) quadratic irreducible: 1/(x^2+1) -> arctan
	test(
		Polynomial([symbolic.from(1)]),
		Polynomial([symbolic.from(1), symbolic.from(0), symbolic.from(1)]), // x^2+1
		symbolic.atan(x)
	);

// 5) polynomial part: (x^2 + x) / 1 -> polynomial integral
	test(
		Polynomial([symbolic.from(0), symbolic.from(1), symbolic.from(1)]), // x + x^2
		Polynomial([symbolic.from(1)]),
		x.ipow(3).scale(1/3).add(x.ipow(2).scale(1/2))
	);

	function test(N: Polynomial<symbolic>, D: Polynomial<symbolic>, expected?: symbolic) {
		const r = rationalT(N, D);
		console.log(`integrate ${r}`)
		const res = integrateRational(N, D, 'x');
		console.log(`result: ${res}`);
		assert(!expected || res === expected);
	}
});

// rothstein tests

// Case definitions
test('rothstein', () => {
	// 1 / (y^2 - 1) = 1 / ((y-1)(y+1))
	test(Polynomial([one]), Polynomial([Polynomial([-1]), zero, one]));

	// t / y - should have global (polynomial) part equal to t
	test(Polynomial([t]), Polynomial([zero, one]));

	// 1 / (y^2 + 1) - irreducible over reals
	test(Polynomial([one]), Polynomial([one, zero, one]));

	// 1 / (y^2 - 2) = 1 / ((y-√2)(y+√2))
	test(Polynomial([one]), Polynomial([Polynomial([-2]), zero, one]));

	// 1 / (y^2 - t) where t is parameter
	test(Polynomial([one]), Polynomial([t.neg(), zero, one]));

	// t / (y - t^2) - linear factor
	test(Polynomial([t]), Polynomial([t.mul(t).neg(), one]));

	// 1 / (y^2 + t + 1) where t is parameter
	test(Polynomial([one]), Polynomial([t.add(one), zero, one]));
	// Behavior depends on t: may be irreducible or split
//	assert(residues.length >= 1, `rothstein7: expected at least 1 factor, got ${residues.length}`);

	function logArgString<T>(arg: Extension<T> | Polynomial<Polynomial<T>>) {
		return 'alpha' in arg
			? `Extension(${arg.alpha.toString('z')}, ${arg.poly.toString('y')})`
			: arg.toString('y');
	}
	function test(N: Polynomial<Polynomial<number>>, D: Polynomial<Polynomial<number>>) {
		console.log(`rothstein: ${N.toString('y')} / ${D.toString('y')}`);
		//const N1 = N.map(c=>c.map(c=>rational.from(c))), D1 = D.map(c=>c.map(c=>rational.from(c)));
		const { polyPart, factors } = partialFractions(N, D);
		console.log(`  polyPart: ${polyPart}`);
		for (const {factor, numerators} of factors) {
			for (const num of numerators) {
				const rt = rothsteinTrager(num, factor);
				for (const r of rt)
					console.log(`  minPoly=${r.minPoly.toString('t')}, mult=${r.multiplicity}, logArg=${logArgString(r.gcd)}`);
			}
		}
	}
});

// Deterministic factorization tests exercising code paths:
// - linear factor with alpha in K
// - extension factor (alpha in K[z])
// - irreducible (no split)
// - product of two linear factors
test('rothsteinFactorsDeterministic', () => {
	console.log('rothsteinFactorsDeterministic');
	const Pr = (arr: number[]) => Polynomial(arr.map(rational.from));

	// modulus R = t^2 - 2
	const R = Pr([-2, 0, 1]);

	// 1) Linear factor over K: S = x - t
	const S1 = Polynomial([Pr([0, -1]), Pr([1])]); // -t + 1*x
	const parts1 = factorOverK_Q(S1.dup(), R);
	const hasLinear = parts1.some(p => !('alpha' in p.factor) && p.factor.degree() === 1 && p.factor.eq(S1));
	if (!hasLinear)
		throw new Error('rothsteinFactorsDeterministic: linear-in-K factor not found');

	// 2) S = x^2 + t*x should split over K = Q[t]/(t^2-2) into two linear factors (x) and (x + t)
	const S2 = Polynomial([Pr([0]), Pr([0, 1]), Pr([1])]); // 0 + t*x + 1*x^2
	const parts2 = factorOverK_Q(S2.dup(), R);


	const tvals = [0, 1, 2];
	for (const tv of tvals) {
		if (Math.abs(R.evaluate(rational.from(tv)).valueOf()) < 1e-12)
			continue;
		const S2num = evalPolyPolyAtT(S2, tv);
		let prodNum = Polynomial([rational(1)]);
		for (const part of parts2) {
			const fnum = evalPartAtT(part.factor, tv);
			prodNum = prodNum.mul(fnum);
		}
		if (!prodNum.eq(S2num))
			throw new Error(`caseFactorsDeterministic: expanded factors do not multiply to S2 at t=${tv}`);
	}

	// 3) Irreducible over K: constant-coeff polynomial x^2 + 1 should remain irreducible
	const S3 = Polynomial([Pr([1]), Pr([0]), Pr([1])]); // 1 + 0*x + 1*x^2
	const parts3 = factorOverK_Q(S3.dup(), R);
	if (parts3.length !== 1 || ('alpha' in parts3[0].factor) || !parts3[0].factor.eq(S3))
		throw new Error('rothsteinFactorsDeterministic: expected irreducible polynomial to remain unsplit');

	// 4) Product of two linear factors: (x - t) * (x - (t+1))
	const L1 = Polynomial([Pr([0, -1]), Pr([1])]); // x - t
	const L2 = Polynomial([Pr([-1, -1]), Pr([1])]); // x - (t+1)
	const S4 = L1.mul(L2);
	const parts4 = factorOverK_Q(S4.dup(), R);
	// multiply returned non-extension factors and compare with S4
	let prod = Polynomial([Pr([1])]);
	for (const p of parts4) {
		if (!('alpha' in p.factor))
			prod = prod.mul(p.factor);
	}
	if (!prod.eq(S4))
		throw new Error('rothsteinFactorsDeterministic: reconstructed product does not match original S4');

	console.log('rothsteinFactorsDeterministic ok');
});