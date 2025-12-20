import { symbolic } from '../src/symbolic';
import { astToRationalExpr, integrateRational } from '../src/rationalExpr';
import { partialFractions2T } from '@isopodlabs/maths/polynomial';

const oneSym = symbolic.from(1);
const denom1 = symbolic.variable('x').npow(2).add(oneSym); // x^2 + 1
const r1 = astToRationalExpr(oneSym, denom1, 'x');
console.log('RationalExpr:', r1);
if (!r1) process.exit(1);
const pf = partialFractions2T(r1);
console.log('PartialFractions:', pf);
const res = integrateRational(r1, 'x');
console.log('Integrate:', res?.toString());
