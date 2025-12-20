import { integrateRational, rationalExpr } from '../src/rationalExpr';
import { Polynomial, squareFreeFactorization } from '@isopodlabs/maths/polynomial';
import rational from '@isopodlabs/maths/rational';
import { symbolic } from '../src/symbolic';

function ok(cond: boolean, msg: string) {
	if (!cond) {
		console.error('FAIL:', msg);
		process.exit(2);
	}
}

console.log('Running integrateRational comprehensive tests...');

{
	const f = Polynomial([symbolic.from(1), symbolic.from(-2), symbolic.from(1)]);
	const g = Polynomial([symbolic.from(-2), symbolic.from(2)]);
	const f1 = f.dup();
	f1.pseudoRemainder(g);
	console.log(String(f1));

	const sf = squareFreeFactorization(f);
	console.log(sf);
}

const x = symbolic.variable('x');

// 1) derivative-multiple: 3*x^2 / (x^3 + 1) -> ln(x^3+1)
{
	const num = Polynomial([symbolic.from(0), symbolic.from(0), symbolic.from(3)]);
	const den = Polynomial([symbolic.from(1), symbolic.from(0), symbolic.from(0), symbolic.from(1)]);
	const res = integrateRational({num, den}, 'x');
	console.log(`case 1 res: ${res}`);
	ok(res !== undefined, 'case 1: expected a result');
	const expected1 = symbolic.log(x.ipow(3).add(symbolic.from(1)));
	ok(res === expected1, 'case 1: expected ln(x^3+1)');
}

// 2) simple linear factor: 1/(x-2) -> ln(x-2)
{
	const num = Polynomial([symbolic.from(1)]);
	const den = Polynomial([symbolic.from(-2), symbolic.from(1)]); // x-2
	const res = integrateRational({num, den}, 'x');
	console.log(`case 2 res: ${res}`);
	ok(res !== undefined, 'case 2: expected a result');
	const expected2 = symbolic.log(x.add(symbolic.from(-2)));
	ok(res === expected2, 'case 2: expected ln(x-2)');
}

// 3) repeated linear factor: 1/(x-1)^2 -> -1/(x-1)
{
	const num = Polynomial([symbolic.from(1)]);
	const den = Polynomial([symbolic.from(1), symbolic.from(-2), symbolic.from(1)]); // (x-1)^2
	const res = integrateRational({num, den}, 'x');
	console.log(`case 3 res: ${res}`);
	ok(res !== undefined, 'case 3: expected a result');
	const expected3 = symbolic.from(-1).mul(x.add(symbolic.from(-1)).ipow(-1));
	ok(res === expected3, 'case 3: expected -1/(x-1)');
}

// 4) quadratic irreducible: 1/(x^2+1) -> arctan
{
	const num = Polynomial([symbolic.from(1)]);
	const den = Polynomial([symbolic.from(1), symbolic.from(0), symbolic.from(1)]); // x^2+1
	const res = integrateRational({num, den}, 'x');
	console.log(`case 4 res: ${res}`);
	ok(res !== undefined, 'case 4: expected a result');
	const expected4 = symbolic.atan(x);
	ok(res === expected4, 'case 4: expected atan(x)');
}

// 5) polynomial part: (x^2 + x) / 1 -> polynomial integral
{
	const num = Polynomial([symbolic.from(0), symbolic.from(1), symbolic.from(1)]); // x + x^2
	const den = Polynomial([symbolic.from(1)]);
	const res = integrateRational({num, den}, 'x');
	console.log(`case 5 res: ${res}`);
	ok(res !== undefined, 'case 5: expected a result');
	const expected5 = x.ipow(3).scale(1/3).add(x.ipow(2).scale(1/2));
	ok(res === expected5, 'case 5: expected polynomial integral');
}

console.log('All integrateRational tests passed.');

// --- symbolic-coefficient smoke test ---
{
	const a = symbolic.sym('a');
	const b = symbolic.sym('b');
	const c = symbolic.sym('c');
	const x = symbolic.variable('x');

	// integrand: (a*x + b) / (x^2 + c)
	const numS = Polynomial([b, a]);
	const denS = Polynomial([c, symbolic.from(0), symbolic.from(1)]);

	const resS = integrateRational({ num: numS, den: denS } as any, 'x');
	console.log('symbolic result:', String(resS));
	if (!resS) {
		console.error('FAIL: integrateRational returned undefined for symbolic coefficients');
		process.exit(2);
	}

	const fSym = x.ipow(2).add(c);
	const C = a.div(symbolic.from(2));
	const sqrtc = c.sqrt();
	const atanArg = x.div(sqrtc);
	const atanTerm = symbolic.atan(atanArg).mul(b.div(sqrtc));
	const expectedS = symbolic.log(fSym).mul(C).add(atanTerm);

	console.log('symbolic expected:', String(expectedS));
	if (String(resS) !== String(expectedS)) {
		console.error('FAIL: symbolic integrand did not match expected', resS, expectedS);
		process.exit(2);
	}

	console.log('symbolic-coefficient integrateRational smoke test passed');
}
