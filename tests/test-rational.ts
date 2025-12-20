import { symbolic } from '../src/symbolic';
import { astToRationalExpr, integrateRational } from '../src/rationalExpr';

console.log('rational test');

const x = symbolic.variable('x');
const oneSym = symbolic.from(1);
const denomSym = x.add(1);
const r = astToRationalExpr(oneSym, denomSym, 'x');
const res = r && integrateRational(r, 'x');
console.log(String(res));

if (!res) {
	console.error('No result');
	process.exit(1);
}

const s = res.toString();
if (!s.includes('log') && !s.includes('ln')) {
	console.error('unexpected result:', s);
	process.exit(2);
}

const r2 = astToRationalExpr(oneSym, x.mul(2), 'x');
console.log(r2);

console.log('OK');
