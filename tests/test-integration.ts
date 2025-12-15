import { symbolic } from '../src/symbolic';
import { parse } from '@isopodlabs/maths/string';

const x = symbolic.variable('x');
const one = symbolic.from(1);

// Test cases - expected results are symbolic expressions

const tests = [
	{ expr: symbolic.tan(x), expected: symbolic.log(symbolic.cos(x).abs()).neg() },
	{ expr: symbolic.tanh(x), expected: symbolic.log(symbolic.cosh(x)) },

	// Reciprocal trig: 1/tan, 1/sin, 1/cos (cot, csc, sec)
	{ expr: one.div(symbolic.tan(x)), expected: symbolic.log(symbolic.sin(x).abs()) },
	{ expr: one.div(symbolic.sin(x)), expected: symbolic.log(one.sub(symbolic.cos(x)).div(symbolic.sin(x)).abs()) },
	{ expr: one.div(symbolic.cos(x)), expected: symbolic.log(one.add(symbolic.sin(x)).div(symbolic.cos(x)).abs()) },

	// Reciprocal hyperbolic: 1/tanh, 1/sinh, 1/cosh (coth, csch, sech)
	{ expr: one.div(symbolic.tanh(x)), expected: symbolic.log(symbolic.sinh(x).abs()) },
	{ expr: one.div(symbolic.sinh(x)), expected: symbolic.log(symbolic.tanh(x.div(2)).abs()) },
	{ expr: one.div(symbolic.cosh(x)), expected: symbolic.atan(symbolic.sinh(x)) },

	// Hyperbolic squared
	// ∫sinh²(x) dx = sinh(x)·cosh(x)/2 - x/2
	{ expr: symbolic.sinh(x).npow(2), expected: symbolic.sinh(x).mul(symbolic.cosh(x)).div(2).sub(x.div(2)) },
	// ∫cosh²(x) dx = sinh(x)·cosh(x)/2 + x/2
	{ expr: symbolic.cosh(x).npow(2), expected: symbolic.sinh(x).mul(symbolic.cosh(x)).div(2).add(x.div(2)) },
	// ∫tanh²(x) dx = x - tanh(x)
	{ expr: symbolic.tanh(x).npow(2), expected: x.sub(symbolic.tanh(x)) },

	// Trig squared
	// ∫sin²(x) dx = x/2 - sin(x)·cos(x)/2
	{ expr: symbolic.sin(x).npow(2), expected: x.div(2).sub(symbolic.sin(x).mul(symbolic.cos(x)).div(2)) },
	// ∫cos²(x) dx = x/2 + sin(x)·cos(x)/2
	{ expr: symbolic.cos(x).npow(2), expected: x.div(2).add(symbolic.sin(x).mul(symbolic.cos(x)).div(2)) },
	// ∫tan²(x) dx = tan(x) - x
	{ expr: symbolic.tan(x).npow(2), expected: symbolic.tan(x).sub(x) },

	// Higher power trig (reduction formulas) - simple argument
	// ∫sin³(x) dx = -sin²(x)·cos(x)/3 + 2/3·∫sin(x) dx = -sin²(x)·cos(x)/3 - 2cos(x)/3
	{ expr: symbolic.sin(x).npow(3) },
	// ∫cos³(x) dx = cos²(x)·sin(x)/3 + 2/3·∫cos(x) dx = cos²(x)·sin(x)/3 + 2sin(x)/3
	{ expr: symbolic.cos(x).npow(3) },
	// ∫sin⁴(x) dx - verify derivative
	{ expr: symbolic.sin(x).npow(4) },
	// ∫cos⁴(x) dx - verify derivative
	{ expr: symbolic.cos(x).npow(4) },

	// Higher power hyperbolic (reduction formulas) - simple argument
	{ expr: symbolic.sinh(x).npow(3) },
	{ expr: symbolic.cosh(x).npow(3) },
	{ expr: symbolic.sinh(x).npow(4) },
	{ expr: symbolic.cosh(x).npow(4) },

	// Higher power trig with complex argument (u-substitution + reduction)
	// ∫2x·sin²(x²) dx
	{ expr: x.mul(2).mul(symbolic.sin(x.npow(2)).npow(2)) },
	// ∫2x·cos²(x²) dx
	{ expr: x.mul(2).mul(symbolic.cos(x.npow(2)).npow(2)) },
	// ∫2x·sin³(x²) dx
	{ expr: x.mul(2).mul(symbolic.sin(x.npow(2)).npow(3)) },
	// ∫2x·cos³(x²) dx
	{ expr: x.mul(2).mul(symbolic.cos(x.npow(2)).npow(3)) },
	// ∫3x²·sin²(x³) dx
	{ expr: x.npow(2).mul(3).mul(symbolic.sin(x.npow(3)).npow(2)) },

	// Higher power hyperbolic with complex argument
	// ∫2x·sinh²(x²) dx
	{ expr: x.mul(2).mul(symbolic.sinh(x.npow(2)).npow(2)) },
	// ∫2x·cosh²(x²) dx
	{ expr: x.mul(2).mul(symbolic.cosh(x.npow(2)).npow(2)) },

	// Trig × Exp products
	// ∫sin(x)·e^x dx = e^x/(1+1)·(1·sin(x) - 1·cos(x)) = e^x·(sin(x) - cos(x))/2
	{ expr: symbolic.sin(x).mul(symbolic.exp(x)), expected: symbolic.exp(x).mul(symbolic.sin(x).sub(symbolic.cos(x))).div(2) },
	// ∫cos(x)·e^x dx = e^x/(1+1)·(1·sin(x) + 1·cos(x)) = e^x·(sin(x) + cos(x))/2
	{ expr: symbolic.cos(x).mul(symbolic.exp(x)), expected: symbolic.exp(x).mul(symbolic.sin(x).add(symbolic.cos(x))).div(2) },

	// Hyperbolic × Exp products
	// ∫sinh(x)·e^(2x) dx = e^(2x)/(4-1)·(2·sinh(x) - cosh(x)) = e^(2x)·(2·sinh(x) - cosh(x))/3
	{ expr: symbolic.sinh(x).mul(symbolic.exp(x.mul(2))), expected: symbolic.exp(x.mul(2)).mul(symbolic.sinh(x).mul(2).sub(symbolic.cosh(x))).div(3) },
	// ∫cosh(x)·e^(2x) dx = e^(2x)/(4-1)·(2·cosh(x) - sinh(x)) = e^(2x)·(2·cosh(x) - sinh(x))/3
	{ expr: symbolic.cosh(x).mul(symbolic.exp(x.mul(2))), expected: symbolic.exp(x.mul(2)).mul(symbolic.cosh(x).mul(2).sub(symbolic.sinh(x))).div(3) },

	// Trig products (different arguments) - use product-to-sum identities
	// ∫sin(2x)·cos(3x) dx = -½[cos(5x)/5 + cos(-x)/(-1)] → ½(5cos(-x) - cos(5x))/5
	{ expr: symbolic.sin(x.mul(2)).mul(symbolic.cos(x.mul(3))), expected: symbolic.cos(x.neg()).mul(5).sub(symbolic.cos(x.mul(5))).div(10) },
	// ∫sin(x)·sin(3x) dx = ½[sin(-2x)/(-2) - sin(4x)/4] → (2sin(2x) - sin(4x))/8
	{ expr: symbolic.sin(x).mul(symbolic.sin(x.mul(3))), expected: symbolic.sin(x.mul(2)).mul(2).sub(symbolic.sin(x.mul(4))).div(8) },
	// ∫cos(2x)·cos(5x) dx = ½[sin(-3x)/(-3) + sin(7x)/7]
	{ expr: symbolic.cos(x.mul(2)).mul(symbolic.cos(x.mul(5))), expected: symbolic.sin(x.mul(-3)).div(-3).add(symbolic.sin(x.mul(7)).div(7)).div(2) },

	// Inverse trig/hyperbolic integrals
	// ∫asin(x) dx = x·asin(x) + sqrt(1-x²)
	{ expr: symbolic.asin(x), expected: x.mul(symbolic.asin(x)).add(one.sub(x.npow(2)).rpow(1, 2)) },
	// ∫acos(x) dx = x·acos(x) - sqrt(1-x²)
	{ expr: symbolic.acos(x), expected: x.mul(symbolic.acos(x)).sub(one.sub(x.npow(2)).rpow(1, 2)) },
	// ∫atan(x) dx = x·atan(x) - ln(1+x²)/2
	{ expr: symbolic.atan(x), expected: x.mul(symbolic.atan(x)).sub(symbolic.log(one.add(x.npow(2))).div(2)) },
	// ∫asinh(x) dx = x·asinh(x) - sqrt(x²+1)
	{ expr: symbolic.asinh(x), expected: x.mul(symbolic.asinh(x)).sub(x.npow(2).add(one).rpow(1, 2)) },
	// ∫acosh(x) dx = x·acosh(x) - sqrt(x²-1)
	{ expr: symbolic.acosh(x), expected: x.mul(symbolic.acosh(x)).sub(x.npow(2).sub(one).rpow(1, 2)) },
	// ∫atanh(x) dx = x·atanh(x) + ln(1-x²)/2
	{ expr: symbolic.atanh(x), expected: x.mul(symbolic.atanh(x)).add(symbolic.log(one.sub(x.npow(2))).div(2)) },


	// atan2 derivative: d/dx atan2(A,B) = (A·dB - B·dA)/(A²+B²)
	// For A=ax+1, B=bx+1: dA=a, dB=b, so derivative = ((ax+1)b - (bx+1)a)/(A²+B²) = (b-a)/(...)
	// Therefore integrand (a-b)/(...) = -(b-a)/(...) gives -atan2(A,B)
	{
		expr:		parse(symbolic, '(a(b.x+1) - b(a.x+1)) / ((a.x+1)^2+(b.x+1)^2)'),
		expected:	parse(symbolic, '-(atan2(a * x + 1, b * x + 1))')
	},
    // Basic cases
	{ expr: symbolic.from(5), expected: symbolic.from(5).mul(x) },
	{ expr: x, expected: x.pow(2).div(2) },
	{ expr: x.pow(2), expected: x.pow(3).div(3) },
	{ expr: x.pow(3), expected: x.pow(4).div(4) },
	
	// Sums
	{ expr: x.add(1), expected: x.pow(2).div(2).add(x) },
	{ expr: x.pow(2).add(x), expected: x.pow(3).div(3).add(x.pow(2).div(2)) },
	
	// Trig functions
	{ expr: symbolic.sin(x), expected: symbolic.cos(x).neg() },
	{ expr: symbolic.cos(x), expected: symbolic.sin(x) },
	{ expr: symbolic.sin(x.mul(2)), expected: symbolic.cos(x.mul(2)).neg().div(2) },
	
	// U-substitution cases
	{ expr: x.mul(2).mul(symbolic.sin(x.pow(2))), expected: symbolic.cos(x.pow(2)).neg() },
	{ expr: x.mul(symbolic.exp(x.pow(2))), expected: symbolic.exp(x.pow(2)).div(2) },
	
	// Exponential
	{ expr: symbolic.exp(x), expected: symbolic.exp(x) },
	{ expr: symbolic.exp(x.mul(2)), expected: symbolic.exp(x.mul(2)).div(2) },
	
	// Integration by parts cases
	// ∫x·e^x dx = x·e^x - e^x
	{ expr: x.mul(symbolic.exp(x)), expected: x.mul(symbolic.exp(x)).sub(symbolic.exp(x)), byParts: true },
	// ∫x·sin(x) dx = -x·cos(x) + sin(x)
	{ expr: x.mul(symbolic.sin(x)), expected: symbolic.cos(x).neg().mul(x).add(symbolic.sin(x)), byParts: true },
	// ∫x·cos(x) dx = x·sin(x) + cos(x)
	{ expr: x.mul(symbolic.cos(x)), expected: x.mul(symbolic.sin(x)).add(symbolic.cos(x)), byParts: true },
	// ∫log(x) dx = x·log(x) - x = (log(x) - 1)·x
	{ expr: symbolic.log(x), expected: symbolic.log(x).sub(1).mul(x), byParts: true },

	// Pow: constant base, variable exponent (exponential rule): ∫n^x dx = n^x / ln(n)
	{ expr: symbolic.pow(2, x), expected: symbolic.pow(2, x).div(symbolic.log(2)) },
	{ expr: symbolic.pow(10, x), expected: symbolic.pow(10, x).div(symbolic.log(10)) },
	{ expr: symbolic.pow(3, x.mul(2)), expected: symbolic.pow(3, x.mul(2)).div(symbolic.log(3)).div(2) },

	// Pow: variable base, constant exponent (power rule): ∫x^n dx = x^(n+1)/(n+1)
	{ expr: symbolic.pow(x, 2), expected: symbolic.pow(x, 3).div(3) },
	{ expr: symbolic.pow(x, symbolic.from(1).div(2)), expected: symbolic.pow(x, symbolic.from(3).div(2)).div(symbolic.from(3).div(2)) },
	{ expr: symbolic.pow(x.mul(2), 3), expected: symbolic.pow(x.mul(2), 4).div(4).div(2) },

	// atan2 with one constant argument
	// ∫atan2(x, c) dx = x·atan2(x, c) - (c/2)·ln(x² + c²)
	{ expr: symbolic.atan2(x, 2), expected: x.mul(symbolic.atan2(x, 2)).sub(symbolic.log(x.npow(2).add(4))) },
	// ∫atan2(c, x) dx = x·atan2(c, x) + (c/2)·ln(x² + c²)
	{ expr: symbolic.atan2(3, x), expected: x.mul(symbolic.atan2(3, x)).add(symbolic.log(x.npow(2).add(9)).scale(1.5)) },

	// Pattern rule tests - using simple x as inner function
	// ∫ 1/sqrt(1-x²) dx = asin(x)
	{ expr: one.div(one.sub(x.npow(2)).rpow(1, 2)), expected: symbolic.asin(x), pattern: 'asin' },
	// ∫ -1/sqrt(1-x²) dx = acos(x) -- note: this is actually -asin(x) since d/dx acos = -d/dx asin
	{ expr: one.neg().div(one.sub(x.npow(2)).rpow(1, 2)), expected: symbolic.asin(x).neg(), pattern: 'acos' },
	// ∫ 1/(1+x²) dx = atan(x)
	{ expr: one.div(one.add(x.npow(2))), expected: symbolic.atan(x), pattern: 'atan' },

	// ∫ 1/sqrt(x²+1) dx = asinh(x)
	{ expr: one.div(x.npow(2).add(one).rpow(1, 2)), expected: symbolic.asinh(x), pattern: 'asinh' },
	// ∫ 1/sqrt(x²-1) dx = acosh(x)
	{ expr: one.div(x.npow(2).sub(one).rpow(1, 2)), expected: symbolic.acosh(x), pattern: 'acosh' },
	// ∫ 1/(1-x²) dx = atanh(x)
	{ expr: one.div(one.sub(x.npow(2))), expected: symbolic.atanh(x), pattern: 'atanh' },

	// ∫ 1/x dx = log(x)
	{ expr: one.div(x), expected: symbolic.log(x), pattern: 'log' },

	// ∫ exp(x)·1 dx = exp(x) - already covered above

	// ∫ cos(x)·1 dx = sin(x) - already covered above

	// ∫ 1/cos²(x) dx = tan(x)
	{ expr: one.div(symbolic.cos(x).npow(2)), expected: symbolic.tan(x), pattern: 'tan' },

	// ∫ cosh(x)·1 dx = sinh(x)
	{ expr: symbolic.cosh(x), expected: symbolic.sinh(x), pattern: 'sinh' },
	// ∫ sinh(x)·1 dx = cosh(x)
	{ expr: symbolic.sinh(x), expected: symbolic.cosh(x), pattern: 'cosh' },
	// ∫ 1/cosh²(x) dx = tanh(x)
	{ expr: one.div(symbolic.cosh(x).npow(2)), expected: symbolic.tanh(x), pattern: 'tanh' },

	// Pattern rule tests with non-trivial inner function A = x², dA = 2x
	// ∫ 2x/sqrt(1-x⁴) dx = asin(x²)
	{ expr: x.mul(2).div(one.sub(x.npow(4)).rpow(1, 2)), expected: symbolic.asin(x.npow(2)), pattern: 'asin(x²)' },
	// ∫ -2x/sqrt(1-x⁴) dx = acos(x²)
	{ expr: x.mul(-2).div(one.sub(x.npow(4)).rpow(1, 2)), expected: symbolic.asin(x.npow(2)).neg(), pattern: 'acos(x²)' },
	// ∫ 2x/(1+x⁴) dx = atan(x²)
	{ expr: x.mul(2).div(one.add(x.npow(4))), expected: symbolic.atan(x.npow(2)), pattern: 'atan(x²)' },

	// ∫ 2x/sqrt(x⁴+1) dx = asinh(x²)
	{ expr: x.mul(2).div(x.npow(4).add(one).rpow(1, 2)), expected: symbolic.asinh(x.npow(2)), pattern: 'asinh(x²)' },
	// ∫ 2x/sqrt(x⁴-1) dx = acosh(x²)
	{ expr: x.mul(2).div(x.npow(4).sub(one).rpow(1, 2)), expected: symbolic.acosh(x.npow(2)), pattern: 'acosh(x²)' },
	// ∫ 2x/(1-x⁴) dx = atanh(x²)
	{ expr: x.mul(2).div(one.sub(x.npow(4))), expected: symbolic.atanh(x.npow(2)), pattern: 'atanh(x²)' },

	// ∫ 2/x dx = 2·log(x) (note: 2x/x² simplifies to 2/x before integration)
	{ expr: x.mul(2).div(x.npow(2)), expected: symbolic.log(x).scale(2), pattern: 'log (simplified)' },

	// ∫ 2x·exp(x²) dx = exp(x²)
	{ expr: x.mul(2).mul(symbolic.exp(x.npow(2))), expected: symbolic.exp(x.npow(2)), pattern: 'exp(x²)' },

	// ∫ 2x·cos(x²) dx = sin(x²)
	{ expr: x.mul(2).mul(symbolic.cos(x.npow(2))), expected: symbolic.sin(x.npow(2)), pattern: 'sin(x²)' },
	// ∫ -2x·sin(x²) dx = cos(x²)
	{ expr: x.mul(-2).mul(symbolic.sin(x.npow(2))), expected: symbolic.cos(x.npow(2)), pattern: 'cos(x²)' },
	// ∫ 2x/cos²(x²) dx = tan(x²)
	{ expr: x.mul(2).div(symbolic.cos(x.npow(2)).npow(2)), expected: symbolic.tan(x.npow(2)), pattern: 'tan(x²)' },

	// ∫ 2x·cosh(x²) dx = sinh(x²)
	{ expr: x.mul(2).mul(symbolic.cosh(x.npow(2))), expected: symbolic.sinh(x.npow(2)), pattern: 'sinh(x²)' },
	// ∫ 2x·sinh(x²) dx = cosh(x²)
	{ expr: x.mul(2).mul(symbolic.sinh(x.npow(2))), expected: symbolic.cosh(x.npow(2)), pattern: 'cosh(x²)' },
	// ∫ 2x/cosh²(x²) dx = tanh(x²)
	{ expr: x.mul(2).div(symbolic.cosh(x.npow(2)).npow(2)), expected: symbolic.tanh(x.npow(2)), pattern: 'tanh(x²)' },

	// With A = sin(x), dA = cos(x)
	// ∫ cos(x)/(1+sin²(x)) dx = atan(sin(x))
	{ expr: symbolic.cos(x).div(one.add(symbolic.sin(x).npow(2))), expected: symbolic.atan(symbolic.sin(x)), pattern: 'atan(sin(x))' },
	// ∫ cos(x)·exp(sin(x)) dx = exp(sin(x))
	{ expr: symbolic.cos(x).mul(symbolic.exp(symbolic.sin(x))), expected: symbolic.exp(symbolic.sin(x)), pattern: 'exp(sin(x))' },
];

console.log('Testing integration...\n');

let passed = 0;
let failed = 0;

//symbolic.setMatchLogging(true);

// Numerical verification for cases where symbolic simplification is insufficient
function numericallyEqual(a: symbolic, b: symbolic, testPoint = 0.7): boolean {
	const av = a.evaluate({x: testPoint, a: 1.1, b: 1.3});
	const bv = b.evaluate({x: testPoint, a: 1.1, b: 1.3});
	return Math.abs(av - bv) < 1e-10;
}

for (const test of tests) {
	const result = test.expr.integral('x');
	const deriv	= result.derivative('x');

	// If expected is provided, check exact match; otherwise verify via derivative
	if (test.expected !== undefined) {
		if (result.eq(test.expected)) {
			passed++;
			console.log(`✓ ∫(${test.expr}) dx = ${result}`);
		} else {
			failed++;
			console.log(`✗ ∫(${test.expr}) dx`);
			console.log(`  Got:      ${result}`);
			console.log(`  Expected: ${test.expected}`);
			console.log(`  Got deriv: ${deriv}`);
		}
	} else {
		// Verify by derivative (symbolically or numerically)
		if (deriv.eq(test.expr) || numericallyEqual(deriv, test.expr)) {
			passed++;
			console.log(`✓ ∫(${test.expr}) dx = ${result}`);
		} else {
			failed++;
			console.log(`✗ ∫(${test.expr}) dx`);
			console.log(`  Got:      ${result}`);
			console.log(`  Got deriv: ${deriv}`);
			console.log(`  Expected deriv: ${test.expr}`);
		}
	}
}

console.log(`\n${passed} passed, ${failed} failed`);
