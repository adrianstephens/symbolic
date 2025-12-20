import { symbolic } from '../src/symbolic';
import { astToRationalExpr, integrateRational } from '../src/rationalExpr';

console.log('rational quadratic test');

const oneSym = symbolic.from(1);
const denom1 = symbolic.variable('x').npow(2).add(oneSym); // x^2 + 1
const r1 = astToRationalExpr(oneSym, denom1, 'x');
const res1 = r1 && integrateRational(r1, 'x');
console.log(String(res1));
if (!res1 || !res1.toString().includes('atan')) {
	console.error('unexpected result for 1/(x^2+1):', String(res1));
	process.exit(1);
}

// test 2x / (1 + x^4) -> atan(x^2)
const denom2 = oneSym.add(symbolic.variable('x').ipow(4));
const numer2 = symbolic.variable('x').mul(2);
const r2 = astToRationalExpr(numer2, denom2, 'x');
const res2 = r2 && integrateRational(r2, 'x');
console.log(String(res2));
if (!res2 || !res2.toString().includes('atan')) {
	console.error('unexpected result for 2x/(1+x^4):', String(res2));
	process.exit(2);
}

console.log('OK');
