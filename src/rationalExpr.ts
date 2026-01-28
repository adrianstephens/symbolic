/* eslint-disable no-restricted-syntax */
import { Polynomial, PolynomialN, PolynomialOver, factorType, squareFreeFactorization, resultantViaPRS, partialFractions, powers, PolyModFactory} from '@isopodlabs/maths/polynomial';
import gen from '@isopodlabs/maths/gen';
import integer from '@isopodlabs/maths/integer';
import rational from '@isopodlabs/maths/rational';
import { symbolic } from './symbolic';
import { fillHoles, copyFillHolesVec, permute, LUDecomposeBareiss, LUSolveBareissTranspose } from '@isopodlabs/maths/linear';

//const zeta	= symbolic.variable('ζ');

//-----------------------------------------------------------------------------
//	Hermite
// Hermite-Ostrogradsky's Algorithm
//-----------------------------------------------------------------------------

// Hermite reduction for Ni(x) / F^i(x)
export function hermiteReduction<T extends factorType>(factor: Polynomial<T>, numerators: Polynomial<T>[]): {derivs: Polynomial<T>[], remainder: Polynomial<T>} {

	const multiplicity = numerators.length;
	if (multiplicity == 1)
		return {derivs:[], remainder: numerators[0]};

	// A(x)/f(x)^k = d(B(x) / f(x)^(k-1)) / dx + C(x) / f(x)^(k-1)
	// A(x) = (k - 1) B(x) f'(x) + f(x) B'(x) + C(x) / f(x)

	const zero	= gen.zero(factor.leadCoeff());
	const one	= gen.from(zero, 1);
	const fPows: Polynomial<T>[] = [];

	for (let j = 0, fpow = Polynomial([one]); j <= multiplicity; j++, fpow = fpow.mul(factor))
		fPows[j] = fpow;

	const b		= numerators.reduce((_b, numerator, j) => _b.add(numerator.mul(fPows[multiplicity - j - 1])), Polynomial([zero]));

	const fDer	= factor.deriv();
	const d		= factor.degree();
	const A: T[][] = [];

	for (let k = 1; k < multiplicity; k++) {
		const temp = fDer.mul(fPows[multiplicity - k - 1]).scale(-k);
		A.push(temp.c);
		for (let t = 1; t < d; t++)
			A.push(fPows[multiplicity - k].shift(t - 1).scale(t).add(temp.shift(t)).c);
	}

	for (let t = 0; t < d; t++)
		A.push(fPows[multiplicity - 1].shift(t).c);

	fillHoles(A, A.length, zero);
	const { perm, rank } = LUDecomposeBareiss(A);
	let x = LUSolveBareissTranspose(A, copyFillHolesVec(b.c, A.length, zero), rank);

	if (x) {
		x = permute(x, perm, zero);
		return {
			derivs: Array.from({length: multiplicity - 1}, (_,i) => Polynomial(x.slice(i * d, (i + 1) * d))),
			remainder: Polynomial(x.slice((multiplicity - 1) * d, multiplicity * d))
		};
	}
	return {derivs:[], remainder: numerators[0]};
}

//-----------------------------------------------------------------------------
//	Rothstein
//-----------------------------------------------------------------------------

export interface rothsteinResult<T> {
	minPoly:	Polynomial<T>;
	gcd:		Polynomial<Polynomial<T>>;
	multiplicity: number;
}

export function rothsteinTrager<T extends factorType>(A: Polynomial<T>, D: Polynomial<T>) {
	const results: rothsteinResult<T>[] = [];

	const zero		= gen.zero(A.leadCoeff());
	const Dd		= D.deriv();
	const Q			= A.map(c => PolynomialOver('z', [c])).sub(Dd.map(c => PolynomialOver('z', [zero, c])));
	if (Q.degree() < 0)
		return results;

	const D1		= D.map(c => PolynomialOver('z', [c]));
	const R			= resultantViaPRS(D1, Q);
	const factors	= squareFreeFactorization(R);

	for (const {factor: minPoly, multiplicity} of factors) {
		// r(z) is an irreducible factor of R(z)
		const mod = PolyModFactory(minPoly);  // Field K = ℚ[z]/(r(z))

		// Compute gcd in K[x]
		const D_mod	= D1.map(c => mod.wrap(c));
		const Q_mod	= Q.map(c => mod.wrap(c));  // reduce poly in z modulo r(z)
		const gcd	= gen.gcd(D_mod, Q_mod).map(c => c.v);  // GCD in K[x]

		results.push({ minPoly, gcd, multiplicity });
	}
	return results;
}

//-----------------------------------------------------------------------------
//	integrateRational
//-----------------------------------------------------------------------------

export function integrateRational(N: Polynomial<symbolic>, D: Polynomial<symbolic>, v = 'x'): symbolic | undefined {
	let	varSym	= symbolic.variable(v);
	let res		= symbolic.zero;

	if (D.degree() <= 0) {
		polyTerm(N.divmod(D));
		return res;
	}

	// try substitution when denominator is a polynomial in x^d and numerator matches du/dx pattern
	if (N.c.slice(0, -1).every(c => c.sign() === 0)) {
		const degs	= powers(D);
		const g		= integer.gcd(...degs);
		const d 	= N.degree();

		if (g > 1 && g % d === 0) {
			const k = N.c[d - 1];
			const reduceDen: symbolic[] = [];
			degs.filter(i => (i % d) === 0).forEach(i => reduceDen[i / d] = D.c[i]);
			N = Polynomial([k.div(k.from(d))]);
			D = Polynomial(reduceDen);
			varSym = varSym.ipow(d);
		}
	}

	const { polyPart, factors } = partialFractions(N, D);
	polyTerm(polyPart);

	function polyTerm(poly: Polynomial<symbolic>) {
		for (const i in poly.c) {
			const power = +i + 1;
			res = res.add(varSym.ipow(power).mul(poly.c[i].div(rational(power))));
		}
	}

	function logTerm(logArg: PolynomialN<symbolic>, coeff: symbolic) {
		res = res.add(coeff.mul(symbolic.log(logArg.evaluate(varSym))));
	}

	for (const { factor, numerators } of factors) {
		const {derivs, remainder} = hermiteReduction(factor, numerators);

		derivs.forEach((d, i) =>
			res = res.add(d.evaluate(varSym).div(factor.ipow(i + 1).evaluate(varSym)))
		);

		if (remainder.degree() < 0)
			continue;

		if (factor.degree() === 1) {
			logTerm(factor.normalise(), remainder.c[0]);
			continue;
		}

		const rt = rothsteinTrager(remainder, factor);
		for (const {minPoly, gcd} of rt) {
			const logs = minPoly.realRoots().map(r => {
				r = r.expand();
				return {alpha: r, logArg: gcd.map(c => c.evaluate(r).expand()).normalise()};	// Substitute z = α
			});
			if (logs.length === 2) {
				const {alpha: alpha0, logArg: logArg0} = logs[0];
				const {alpha: alpha1, logArg: logArg1} = logs[1];
				
				if (alpha0 === alpha1.conj()) {
					//atan
					const c = logArg0.c[0] || symbolic.zero;
					const c_re = symbolic.re(c);
					const c_im = symbolic.im(c);
					
					if (c_im === symbolic.zero) {
						// d is real: simpler case; log|c + dx| (need to handle sign)
						logTerm(logArg0, alpha0.mul(2));
					} else {
						// For complex d |g(x)| = √((c + d_r·x)² + (d_i·x)²), arg(g(x)) = arctan(d_i·x / (c + d_r·x))
						res = res.add(symbolic.re(alpha0).mul(symbolic.log(c_re.add(varSym).pow(2).add(c_im.pow(2)))));
						res = res.add(symbolic.im(alpha0).mul(-2).mul(symbolic.atan(varSym.div(c_im))));
					}
					continue;

				} else if (alpha0 === alpha1.neg() && logArg0.c[0] === logArg1.c[0].neg()) {
					//atanh
					res = res.add(alpha0.mul(2).mul(symbolic.atanh(varSym).div(logArg0.c[0])));
					continue;
				}
			}
			for (const {alpha, logArg} of logs)
				logTerm(logArg, alpha);
		}
	}
	
	return res;
}
