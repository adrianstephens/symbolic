import { integrateRational } from '../src/rationalExpr';
import { Polynomial } from '@isopodlabs/maths/polynomial';
import { symbolic } from '../src/symbolic';

function fail(msg: string, ...rest: any[]) {
  console.error(msg, ...rest);
  process.exit(2);
}

const a = symbolic.sym('a');
const b = symbolic.sym('b');
const c = symbolic.sym('c');
const x = symbolic.variable('x');

// integrand: (a*x + b) / (x^2 + c)
const num = Polynomial([b, a]);
const den = Polynomial([c, symbolic.from(0), symbolic.from(1)]);

const res = integrateRational({ num, den } as any, 'x');
console.log('result:', String(res));
if (!res) fail('integrateRational returned undefined for symbolic coefficients');

// expected: (a/2)*ln(x^2 + c) + b / sqrt(c) * atan(x / sqrt(c))
const fSym = x.ipow(2).add(c);
const C = a.div(symbolic.from(2));
const sqrtc = c.sqrt();
const atanArg = x.div(sqrtc);
const atanTerm = symbolic.atan(atanArg).mul(b.div(sqrtc));
const expected = symbolic.log(fSym).mul(C).add(atanTerm);

console.log('expected:', String(expected));
if (String(res) !== String(expected)) fail('symbolic integrand did not match expected', res, expected);

console.log('symbolic-coefficient integrateRational smoke test passed');
