import { Polynomial, partialPower, partialFractionsT, hermiteReduce, RationalPolynomial } from '@isopodlabs/maths/polynomial';
import rational from '@isopodlabs/maths/rational';
import { symbolic } from './symbolic';

export function rationalExpr(num: Polynomial<rational>, den: Polynomial<rational>): RationalPolynomial<rational> {
	// normalize denom to be monic if possible (divide both by lead coeff)
	const lc = den.leadCoeff();
	if (lc && !lc.is1())
		return { num: num.rscale(lc), den: den.rscale(lc) };
	return { num, den };
}

function extractPolynomial(sym: symbolic, varName = 'x'): Polynomial<symbolic> | undefined {
	const groups = sym.collect(symbolic.variable(varName));
	return Polynomial(groups);//.map(g => g?.asNumeric() ?? rational(0)));
}

export function astToRationalExpr(num: symbolic, den: symbolic, varName = 'x'): RationalPolynomial<symbolic> | undefined {
	const numerPoly = extractPolynomial(num, varName);
	const denomPoly = extractPolynomial(den, varName);
	if (numerPoly && denomPoly)
		return { num: numerPoly, den: denomPoly };
}

// Integrate general rational (handles polynomial part + simple real roots)
export function integrateRational(r: RationalPolynomial<symbolic>, v = 'x'): symbolic | undefined {
	// handle trivial denominator (pure polynomial) quickly
	if (r.den.degree() <= 0) {
		const polyPart = r.num.divmod(r.den);
		const varSym = symbolic.variable(v);
		let res = symbolic.zero;
		for (const i in polyPart.c) {
			const coef = polyPart.c[i];
			const power = +i + 1;
			res = res.add(varSym.ipow(power).mul(coef.div(rational(power))));
		}
		return res;
	}

	// first try Hermite reduction to extract exact derivative parts and produce a square-free remainder
	let herm = hermiteReduce(r, symbolic.one);
	// validate hermite result (must have a non-empty denominator); otherwise ignore hermite output
	if (herm && (!herm.remainder || !herm.remainder.den || (herm.remainder.den.c.length === 0)))
		herm = undefined;

	let polyPart: Polynomial<symbolic> | undefined;
	let derivativeTerms: RationalPolynomial<symbolic>[] = [];
	let remainder: RationalPolynomial<symbolic> | undefined;

	if (herm) {
		polyPart = herm.polyPart;
		derivativeTerms = herm.derivative;
		remainder = herm.remainder;
	} else {
		const resPF = partialFractionsT(r);
		if (!resPF)
			return undefined;
		polyPart = resPF.polyPart;
		derivativeTerms = [];
		remainder = { num: r.num, den: r.den };
	}

	const power = partialPower(remainder);
	const resPF2 = partialFractionsT(remainder);
	if (!resPF2)
		return undefined;

	const { terms } = resPF2;
	const varSym = power ? symbolic.variable(v).ipow(power) : symbolic.variable(v);

	// integrate polynomial part

	let res = symbolic.zero;
	for (const i in polyPart.c) {
		const coef = polyPart.c[i];
		const power = +i + 1;
		res = res.add(varSym.ipow(power).mul(coef.div(rational(power))));
	}

	// first add integrals of derivative terms (their integral is just num/den)
	for (const dt of derivativeTerms) {
		// build symbolic numerator and denominator
		let numerSym = symbolic.zero;
		for (const i in dt.num.c) {
			const coef = dt.num.c[i];
			const term = varSym.ipow(+i).mul(coef);
			numerSym = numerSym.add(term);
		}
		let denomSym = symbolic.zero;
		for (const i in dt.den.c) {
			const coef = dt.den.c[+i];
			const term = varSym.ipow(+i).mul(coef);
			denomSym = denomSym.add(term);
		}
		if (numerSym && denomSym)
			res = res ? res.add(numerSym.div(denomSym)) : numerSym.div(denomSym);
	}

	// integrate partial fraction terms from the (square-free) remainder: support linear factors and quadratic irreducibles
	for (const t of terms) {
		const f = t.factor;
		const d = f.degree();

		if (d === 1) {
			// numerator is constant
			const a = f.leadCoeff();
			const c0 = t.numer.c[0] ?? symbolic.zero;
			const root = f.c[0].neg().div(a);
			res = res.add(t.order === 1
				? symbolic.log(varSym.add(root.neg())).mul(c0.div(a))
				: varSym.add(root.neg()).ipow(-(t.order - 1)).mul(c0.div(rational(-(t.order - 1))))
			);
			// linear factor handled fully above for order===1; avoid duplicate handling below
			if (t.order === 1)
				continue;

		} else if (d === 2 && t.order === 1) {
			// numerator is linear or constant (degree <= 1): represent numer = C * f' + D
			const a			= f.leadCoeff();
			const b			= f.c[1] ?? symbolic.zero;
			const c			= f.c[0];
			const A			= t.numer.c[1] ?? symbolic.zero;
			const B			= t.numer.c[0];
			// f'(x) = 2a x + b
			// solve A x + B = C*(2a x + b) + D  => compare coefficients
			const C			= A.div(a.mul(rational(2))).div(rational(1));
			const D			= B.sub(C.mul(b));
			// integral: C * ln(f) + D * integral(1/f)
			// build ln(f)
			const fSym		= a.mul(varSym.ipow(2)).add(b.mul(varSym)).add(c);
			const lnExpr	= symbolic.log(fSym).mul(C);
			// integral of 1/f -> arctan term
			const disc		= a.mul(c).mul(rational(4)).sub(b.mul(b));
			const sqrtDisc	= disc.sqrt();
			const atanArg	= varSym.mul(a.mul(rational(2))).add(b).div(sqrtDisc);
			const atanTerm	= symbolic.atan(atanArg).mul(D.mul(rational(2))).div(sqrtDisc);
			res = res.add(lnExpr.add(atanTerm));
			// already handled this term (quadratic linear-numerator case)
			// skip the generic "additional support" check below to avoid duplicating the log
			continue;
		} else {
			// unsupported factor integration
			return undefined;
		}

		// Additional support: if the numerator is a scalar multiple of the factor derivative
		// then integral is scalar * ln(f) for any degree (order must be 1).
		if (t.order === 1) {
			const fDer = f.deriv();
			// helper: check if numer = c * fDer for some scalar c
			function scalarMultipleOf(a: Polynomial<symbolic>, b: Polynomial<symbolic>): symbolic | null {
				// degrees must match (allow leading zeros)
				const maxd = Math.max(a.degree(), b.degree());
				let c: symbolic | null = null;
				for (let i = 0; i <= maxd; i++) {
					const ai = a.c[i];
					const bi = b.c[i];
					if (!bi) {
						if (ai)
							return null;
						continue;
					}
					const thisC = ai.div(bi);
					if (c === null)
						c = thisC;
					else if (!thisC.eq(c))
						return null;
				}
				return c;
			}

			const c = scalarMultipleOf(t.numer, fDer);
			if (c) {
				// build symbolic representation of f
				let fSymExpr = symbolic.zero;
				for (const i in f.c)
					fSymExpr = fSymExpr.add(varSym.ipow(+i).mul(f.c[i]));
				res = res.add(symbolic.log(fSymExpr).mul(c));
				continue;
			}
		}
	}
	return res;
}
