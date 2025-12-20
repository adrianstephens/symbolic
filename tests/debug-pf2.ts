import { symbolic } from '../src/symbolic';
import { astToRationalExpr, integrateRational } from '../src/rationalExpr';
import { partialFractions2T } from '@isopodlabs/maths/polynomial';

const oneSym = symbolic.from(1);
const denom2 = oneSym.add(symbolic.variable('x').ipow(4));
const numer2 = symbolic.variable('x').mul(2);
const r2 = astToRationalExpr(numer2, denom2, 'x');
console.log('numer2 AST:', numer2.toString());
console.log('numer2.collect:', numer2.collect(symbolic.variable('x')).map(g => String(g)));
console.log('R2:', r2 && r2.num.toString(), r2 && r2.den.toString());
if (!r2) process.exit(1);
const pf = partialFractions2T(r2);
console.log('PF:', pf);
const res = integrateRational(r2, 'x');
console.log('Integrate:', res?.toString());
