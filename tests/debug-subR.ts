import { Polynomial } from '@isopodlabs/maths/polynomial';
import rational from '@isopodlabs/maths/rational';
import { integrateRational } from '../src/rationalExpr';

const numer = Polynomial([rational(1)]);
const denom = Polynomial([rational(1), rational(0), rational(1)]);
const res = integrateRational({ num: numer, den: denom }, 'u');
console.log('res:', res?.toString());
