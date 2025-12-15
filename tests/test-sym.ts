/* eslint-disable @typescript-eslint/no-unused-vars */

import { test, expect, assert } from './test';
import real from '@isopodlabs/maths/real';
import { outputNumber, radicalChars, fractionChars, parse } from '@isopodlabs/maths/string';
import { symbolic, term, factor, mulFactors } from '../dist/symbolic';
import { applyRules, trigRules, invTrigRules, generalRules, scoreFactory, factored } from '../dist/symbolicRules';
import { applyRulesEgraph, EGraphOptions, startSimplify, simplify } from '../dist/egraph';
import { Polynomial } from '@isopodlabs/maths/polynomial';
import { vscalar, vectorT, normalise, mat, E2, E3, E4, E5 , vector, float2, float3, float4, float2x2, float3x3, float4x4} from '@isopodlabs/maths/vector';
import complex from '@isopodlabs/maths/complex';

//symbolic.setDefaultStringifyOptions({ccode: true, radicalPower: true, superPower: true});
symbolic.setDefaultStringifyOptions({
	superPower: true,
	radicalPower: true,
	mulChar: '⋅',
	//printConst: n=>outputNumber(n, {fractions: {chars: fractionChars, superSub: true}, radicals: radicalChars})
});

function makeFunction(expr: symbolic, var1: string) {
	return (x: number) => expr.evaluate({ [var1]: x });
}

function makeDeriv(f: (x: number) => number, h = 1e-6) {
	return (x: number) => (f(x + h) - f(x - h)) / (2 * h);
}

function checkDeriv(expr: symbolic, varName: string, testPoints = [0], h = 1e-6) {
	const deriv = expr.derivative(varName);
	const D0 = makeFunction(deriv, varName);
	const D1 = makeDeriv(makeFunction(expr, varName), h);

	for (const x of testPoints) {
		const d0 = D0(x);
		const d1 = D1(x);
		if (!real.approx(d0, d1, h))
			return false;
	}
	return true;
}

function approxEqualEval(x: symbolic, y: symbolic, varNames: string[], testPoints: Iterable<number[]> = [[0.1], [0.5], [1.2], [-0.7]], tol = 1e-6) {
	for (const v of testPoints) {
		const env = Object.fromEntries(varNames.map((name, i) => [name, v[i % v.length]]));
		const xv = x.evaluate(env);
		const yv = y.evaluate(env);
		if (!real.approx(xv, yv, tol))
			return false;
	}
	return true;
}

function countNodes(n: symbolic) {
	let cnt = 0;
	n.visit({post: x => { cnt++; return x; }});
	return cnt;
}

test('transitive merge', () => {
	const p1 = parse(symbolic, 'a1 + b');
	const p2 = parse(symbolic, 'a2 + b');

	const sim = startSimplify();
	sim.addSymbolic(p1);
	sim.addSymbolic(p2);
	sim.rewrite(parse(symbolic, 'a2'), parse(symbolic, 'a1'));

	const p1e = sim.eclassOfNode(p1);
	const p2e = sim.eclassOfNode(p2);
	assert(p1e === p2e);

	const xbest = sim.extractBest(p1, scoreFactory());
	console.log(String(xbest));
	const ybest = sim.extractBest(p2, scoreFactory());
	console.log(String(ybest));
});

test('canonical reuse on insert after merge', () => {
	const p1 = parse(symbolic, 'a1 + b');
	const p2 = parse(symbolic, 'a2 + b');

	const sim = startSimplify();
	// add p1 first
	sim.addSymbolic(p1);
	// create a2 and immediately merge a2 -> a1 before adding p2
	sim.rewrite(parse(symbolic, 'a2'), parse(symbolic, 'a1'));
	// now add p2 (which references a2 that has been merged into a1)
	sim.addSymbolic(p2);

	const e1 = sim.eclassOfNode(p1);
	const e2 = sim.eclassOfNode(p2);
	assert(e1 === e2, 'Expected parent classes to be the same after inserting p2 post-merge');
});

test('chain merge canonicalMap stability', () => {
	const p1 = parse(symbolic, 'a1 + b');
	const p2 = parse(symbolic, 'a2 + b');
	const p3 = parse(symbolic, 'a3 + b');

	const sim = startSimplify();
	sim.addSymbolic(p1);
	sim.addSymbolic(p2);
	sim.addSymbolic(p3);

	// merge a2 -> a1, then a3 -> a2 (transitively a3 -> a1)
	sim.rewrite(parse(symbolic, 'a2'), parse(symbolic, 'a1'));
	sim.rewrite(parse(symbolic, 'a3'), parse(symbolic, 'a2'));

	const e1 = sim.eclassOfNode(p1);
	const e2 = sim.eclassOfNode(p2);
	const e3 = sim.eclassOfNode(p3);

	assert(e1 === e2, 'p1 and p2 should be merged');
	assert(e1 === e3, 'p1 and p3 should be merged transitively');
});

test('cbrt', () => {
	const t1 = parse(symbolic, '(4(A+B))^(1/3)');
	const t2 = parse(symbolic, '4^(1/3)*(A+B)^(1/3)');

	console.log(t1 === t2);

});


test('bug', () => {
	const t = parse(symbolic, '1⁄2cos(b)²⋅sin(2a)');
	const u = t.expand();
	const t2 = parse(symbolic, '2cos(a)⋅sin(a)');
	const rule = invTrigRules.find(i => i.name === 'double-angle-sin-compress');
	if (rule) {
		const bs = rule.match(t2);
		if (bs)
			console.log(String(rule.replace(bs)));
	}

	const bindings = { a: 1, b: 2, c: 3, d: 4 }
	const validate = (a: symbolic, b: symbolic) => {
		const av = a.evaluate(bindings);
		const bv = b.evaluate(bindings);
		if (av !== bv)
			console.log("uh");
		return av == bv;
	}
	const opts = {maxRounds: 8, verbose: false, maxExpansions: 16, validate};

	const x = parse(symbolic, '(-a*d + b*c)/(a*d - b*c)');
	const y = parse(symbolic, '(-a*d + b*c)+(a*d - b*c)');
	const z = parse(symbolic, '((-a * d + b * c) / (a * d - b * c) + 1) * b / a');

	const eqn0 = parse(symbolic, '(b * c / (a * d - b * c) / a + 1 / a) * a - b * c / (a * d - b * c)');
	const simp0 = applyRulesEgraph(eqn0, generalRules, opts);

	const eqn1 = parse(symbolic, '(b * c / (a * d - b * c) + 1) / a - d / (a * d - b * c)');
	const b = generalRules.find(i => i.name === 'sub-over-denom')?.match(eqn1);

//	const eqn = parse(symbolicOperators, '(b * c / (a * d - b * c) / a + 1 / a) * b - b * d / (a * d - b * c)');
//	const simp = applyRulesEgraph(eqn, generalRules, opts);
//	assert(simp === symbolic.zero);


});

test('complex', () => {
	const z = parse(symbolic, '1 + 2i');
	const t1 = symbolic.conj(z);
	const t2 = symbolic.re(z);
	const t3 = symbolic.im(z);
	const targ = parse(symbolic, 'arg(1 + 2i)');

	const t4 = parse(symbolic, 're(A^2+B) + im(B^3+A)');
	const t5 = parse(symbolic, 'arg(A)');
	const t5d = t5.derivative('A');
	const t5e = factored(t5d);
});

test('symbolic', () => {
	const t = parse(symbolic, 'sqrt(2)');

	const x = symbolic.sym("x");
	const y = symbolic.sym("y");
	const z = symbolic.sym("z");

	const sum = x.add(y);
	const sum2 = sum.add(z);
	expect(sum.toString()).toEqual('x + y');
	expect(sum2.toString()).toEqual('x + y + z');

	const comp = sum2.less(x.mul(2));
	const comp2 = comp.and(comp.not());
	const comp3 = comp.or(comp.not());

	const part = x.greater(0).then(
		() => x.mul(2), 
		() => x.less(0).then(
			() => x.mul(-2),
			() => symbolic.one
		)
	);

	const e2 = sum.div(sum2);
	const d2 = factored(e2.derivative("x"));
	expect(d2).toEqual(z.div(sum2.pow(2)));

	const j = symbolic.i.scale(3).add(2);
	const j2 = j.mul(j).expand();
	expect(j2.toString()).toEqual('12𝑖 - 5');

	const e3 = x.pow(2).add(y.pow(2)).add(z.pow(2));

	const sin = symbolic.sin(e3);
	const dsin = sin.derivative("x");
	expect(dsin).toEqual(parse(symbolic, '2 x cos(x² + y² + z²)'));
});

test('symbolic transforms', () => {
	// expand sin(a+b)*cos(a-b) into combination using product-to-sum then simplify
	const a = symbolic.sym('a');
	const b = symbolic.sym('b');

	const test = parse(symbolic, '(cos(a) * cos(b) + sin(a) * sin(b)) * (cos(a) * sin(b) + cos(b) * sin(a))');
	const test2 = test.expand();
	const test3 = factored(test2);
	const bs = generalRules.find(r => r.name === 'factor-common')?.match(test2);
		

	// sin(a+b)*cos(a-b) = 1/2[sin(2a) + sin(2b)] (using trig identities)
	const expr = symbolic.sin(a.add(b)).mul(symbolic.cos(a.sub(b)));
	const expected = symbolic.sin(a.scale(2)).add(symbolic.sin(b.scale(2))).scale(0.5);

	// Apply egraph with available rules (symbolic exposes a list of rules)
	const rules = trigRules.concat(invTrigRules).concat(generalRules);
	const bindings = {a: 1, b: 2};
	const validate = (a: symbolic, b: symbolic) => {
		const va = a.evaluate(bindings), vb = b.evaluate(bindings);
		if (!real.approx(va, vb, 1e-8))
			return false;
		return true;
	};
	const out = applyRulesEgraph(expr, rules, {maxRounds: 8, maxExpansions: 16, validate});//, { verbose: true, debugNode: 'replace'});
	console.log('Original  :', String(expr));
	console.log('Extracted :', String(out));
	if (!approxEqualEval(out, expr, ['a', 'b']))
		throw new Error('egraph extraction is not equivalent to original expression');

	const x = symbolic.sym("x");
	const y = symbolic.sym("y");
	const sum = x.add(y);

	const sindif = applyRules(symbolic.sin(x.sub(y)), trigRules);
	expect(sindif).toEqual(symbolic.sin(x).mul(symbolic.cos(y)).sub(symbolic.cos(x).mul(symbolic.sin(y))));
	const sinsum = symbolic.sin(sum);
	const sinsum1 = applyRules(sinsum, trigRules);
	expect(sinsum1).toEqual(symbolic.sin(x).mul(symbolic.cos(y)).add(symbolic.cos(x).mul(symbolic.sin(y))));

	const esinsum = applyRulesEgraph(sinsum, trigRules, {scorer: node => -countNodes(node), verbose: false});
	expect(esinsum).toEqual(sinsum1);

	const sinsum2 = applyRules(sinsum1, invTrigRules);
	// structural equality may vary; assert numeric equivalence at sample points
	if (!approxEqualEval(sinsum2, sinsum, ['x', 'y'], [[0, 0], [0.3, 0.7], [1.2, -0.5]]))
		throw new Error('egraph extraction is not equivalent to original expression');

	const sinhalf = symbolic.sin(sum.scale(0.5));
	const sinhalf1 = applyRules(sinhalf, trigRules);
	expect(sinhalf1).toEqual((symbolic.sin(x).mul(symbolic.sin(y)).sub(symbolic.cos(x).mul(symbolic.cos(y))).add(1)).pow(0.5).scale(Math.sqrt(0.5)));

});

test('symbolic trig/hyperbolic', () => {
	const x = symbolic.sym("x");
	const y = symbolic.sym("y");

	// additional function checks
	const a_asin = symbolic.asin(x);
	expect(a_asin.evaluate({ x: 0.5 }), 'asin eval').check(v => Math.abs(v - Math.asin(0.5)) < 1e-12);
	const da_asin = a_asin.derivative('x');
	expect(da_asin.evaluate({ x: 0.5 }), 'asin deriv').check(v => Math.abs(v - 1 / Math.sqrt(1 - 0.5 * 0.5)) < 1e-12);

	const a_acos = symbolic.acos(x);
	expect(a_acos.evaluate({ x: 0.5 }), 'acos eval').check(v => Math.abs(v - Math.acos(0.5)) < 1e-12);
	const da_acos = a_acos.derivative('x');
	expect(da_acos.evaluate({ x: 0.5 }), 'acos deriv').check(v => Math.abs(v - (-1 / Math.sqrt(1 - 0.5 * 0.5))) < 1e-12);

	const a_atan = symbolic.atan(x);
	expect(a_atan.evaluate({ x: 1 }), 'atan eval').check(v => Math.abs(v - Math.atan(1)) < 1e-12);
	const da_atan = a_atan.derivative('x');
	expect(da_atan.evaluate({ x: 1 }), 'atan deriv').check(v => Math.abs(v - (1 / (1 + 1 * 1))) < 1e-12);

	const a_atan2 = symbolic.atan2(y, x);
	expect(a_atan2.evaluate({ x: 2, y: 1 }), 'atan2 eval').check(v => Math.abs(v - Math.atan2(1, 2)) < 1e-12);
	const da_atan2_dx = a_atan2.derivative('x');
	// derivative w.r.t x: -y / (x^2 + y^2)
	expect(da_atan2_dx.evaluate({ x: 2, y: 1 }), 'atan2 dx').check(v => Math.abs(v - (-1 / (4 + 1))) < 1e-12);

	const a_sinh = symbolic.sinh(x);
	expect(a_sinh.evaluate({ x: 0.5 }), 'sinh eval').check(v => Math.abs(v - Math.sinh(0.5)) < 1e-12);
	const da_sinh = a_sinh.derivative('x');
	expect(da_sinh.evaluate({ x: 0.5 }), 'sinh deriv').check(v => Math.abs(v - Math.cosh(0.5)) < 1e-12);

	const a_cosh = symbolic.cosh(x);
	expect(a_cosh.evaluate({ x: 0.5 }), 'cosh eval').check(v => Math.abs(v - Math.cosh(0.5)) < 1e-12);
	const da_cosh = a_cosh.derivative('x');
	expect(da_cosh.evaluate({ x: 0.5 }), 'cosh deriv').check(v => Math.abs(v - Math.sinh(0.5)) < 1e-12);

	const a_tanh = symbolic.tanh(x);
	expect(a_tanh.evaluate({ x: 0.5 }), 'tanh eval').check(v => Math.abs(v - Math.tanh(0.5)) < 1e-12);
	const da_tanh = a_tanh.derivative('x');
	expect(da_tanh.evaluate({ x: 0.5 }), 'tanh deriv').check(v => Math.abs(v - (1 - Math.tanh(0.5) ** 2)) < 1e-12);

});

test('canonicalization merges powers and derivatives simplify', () => {
	const x = symbolic.sym("x");

	const c = x.mul(2).add(x.mul(3));
	const d = x.mul(5);
	expect(c).toEqual(d);

	const a = x.pow(2).mul(x.pow(3));
	const b = x.pow(5);
	expect(a).toEqual(b);

	const da = a.derivative("x");
	expect(da.evaluate({ x: 2 }), 'derivative value').toEqual(5 * Math.pow(2, 4));

	// more complex: x^2 * (x+1) * x^3 -> x^5*(x+1)
	const expr = x.pow(2).mul(x.add(1)).mul(x.pow(3));
	expect(expr).toEqual(x.pow(5).mul(x.add(1)));

	checkDeriv(expr, 'x', [3]);
});

test('expand distributes multiplication over addition (single additive factor)', () => {
	const x = symbolic.sym('x');
	const y = symbolic.sym('y');

	const expr = x.add(1).mul(y);
	const expanded = expr.expand({});

	const expected = x.mul(y).add(y);
	expect(expanded).toEqual(expected);
});

test('expand distributes multiplication over addition (two additive factors)', () => {
	const x = symbolic.sym('x');
	const y = symbolic.sym('y');

	const expr = x.add(1).mul(y.add(2));
	const expanded = expr.expand({});

	// (x+1)*(y+2) -> x*y + 2*x + y + 2
	expect(expanded).toEqual(x.mul(y).add(x.mul(2)).add(y).add(2));
});

test('expand distributes multiplication over powers)', () => {
	const x = symbolic.sym('x');

	const expr = x.add(1).pow(2);
	const expanded = expr.expand({});
	const expected = x.pow(2).add(x.mul(2)).add(1);

	// (x+1)^2 -> x^2 + 2*x + 1
	expect(expanded).toEqual(expected);
});

test('collect groups terms by variable power (no expand)', () => {
	const x = symbolic.sym('x');
	const y = symbolic.sym('y');

	// x^2 * y + 2 * x * y + 3
	const expr = x.pow(2).mul(y).add(x.mul(y).mul(2)).add(3);
	const groups = expr.collect('x');
	const pg = Polynomial(groups).evaluate(x).expand();

	// groups is a sparse array: index = power, value = coefficient (symbolic)
	expect(pg).toEqual(expr);
});

test('collect after expand (x+1)^2 * y', () => {
	const x = symbolic.sym('x');
	const y = symbolic.sym('y');

	const expr = x.add(1).pow(2).mul(y);
	const expanded = expr.expand({});
	const groups = expanded.collect('x');
	const pg = Polynomial(groups).evaluate(x).expand();

	expect(pg).toEqual(expanded);
});


test('egraph smoke', () => {
	const x = symbolic.sym('x');
	const y = symbolic.sym('y');
	const out = applyRulesEgraph(symbolic.sin(x.add(y)), trigRules, {scorer: node => -countNodes(node)});
	expect(typeof out.id).toEqual('string');
});

test('egraph general expand+collect simplification', () => {
	const x = symbolic.sym('x');
	// expr = (x+1)*(x+2) + (x+1)*x  => should factor to (x+1)*(2x+2)
	const expr = x.add(1).mul(x.add(2)).add(x.add(1).mul(x));
	const out = applyRulesEgraph(expr, generalRules);

	// expected factorised form (x+1)*(x+2 + x) == (x+1)*(2x+2)
	const expected = x.add(1).mul(x.add(2).add(x));

	// assert the extractor chose the expected factored form (structural equality)
	expect(out).toEqual(expected);
});

test('egraph ambitious factorisations (scoreFactory params)', () => {
	const x = symbolic.sym('x');
	const y = symbolic.sym('y');

	// Example 1: three terms sharing (x+1)
	// (x+1)*(x+2) + (x+1)*(x+3) + (x+1)*x  => (x+1)*(3*x + 5)
	const expr1 = x.add(1).mul(x.add(2)).add(x.add(1).mul(x.add(3))).add(x.add(1).mul(x));
	const expected1 = x.add(1).mul(x.mul(3).add(5));

	// Example 2: mixed variables, factor (x+1) across different y-terms
	// (x+1)*(y+2) + (x+1)*(y+3) => (x+1)*(2*y + 5)
	const expr2 = x.add(1).mul(y.add(2)).add(x.add(1).mul(y.add(3)));
	const expected2 = x.add(1).mul(y.mul(2).add(5));

	// Use a conservative recommended scorer (exposed constW) to encourage factoring
	const scorer = scoreFactory(0.2, 0.5, 1, 0.1);

	const out1 = applyRulesEgraph(expr1, generalRules, {scorer});
	const out2 = applyRulesEgraph(expr2, generalRules, {scorer});

	// assert the extractor chose the expected factored forms (structural equality)
	expect(out1).toEqual(expected1);
	expect(out2).toEqual(expected2);
});

//Α α, Β β, Γ γ, Δ δ, Ε ε, Ζ ζ, Η η, Θ θ, Ι ι, Κ κ, Λ λ, Μ μ, Ν ν, Ξ ξ, Ο ο, Π π, Ρ ρ, Σ σ ς, Τ τ, Υ υ, Φ φ, Χ χ, Ψ ψ, Ω ω

function vecVisit<T extends vscalar<T>, R extends string>(v: vector<R, T>, visitor: (node: T) => T) {
	return v.create(...v._values.map(s => visitor(s)));
}

function matVisit<T extends vscalar<T>, C extends string, R extends string>(m: mat<vector<R, T>, C>, visitor: (node: T) => T) {
	return m.create(...m.columns().map(c => vecVisit(c, visitor)));
}

test('vector perp()', () => {
	const a = symbolic.sym('a');
	const b = symbolic.sym('b');
	const c = symbolic.sym('c');
	const d = symbolic.sym('d');

	const v = vectorT(E2, a, b);
	const v1 = normalise(v);
	console.log(v1.x.toString({radicalPower: true, superPower: true}));
	const d0 = v.dot(v);
	const d1 = factored(v1.dot(v1));


	const m2x2 = mat({
		x: vectorT(E2, a, b),
		y: vectorT(E2, c, d)
	});

	const m3x3 = mat({
		x: vectorT(E3, a, b, c),
		y: vectorT(E3, d, a, b),
		z: vectorT(E3, c, d, a)
	});
	const dt3 = m3x3.det();
	const dx = dt3.expand({uses: new Set(['c'])});

	const m2 = m3x3.matmul(m3x3);

	const θ = symbolic.sym('θ');

	const rZ = mat({
		x: vectorT(E3, symbolic.cos(θ), symbolic.sin(θ), symbolic.zero),
		y: vectorT(E3, symbolic.sin(θ).neg(), symbolic.cos(θ), symbolic.zero),
		z: vectorT(E3, symbolic.zero, symbolic.zero, symbolic.one)
	});

	const rY = mat({
		x: vectorT(E3, symbolic.cos(θ), symbolic.zero, symbolic.sin(θ).neg()),
		y: vectorT(E3, symbolic.zero, symbolic.one, symbolic.zero),
		z: vectorT(E3, symbolic.sin(θ), symbolic.zero, symbolic.cos(θ))
		});

	const rX = mat({
		x: vectorT(E3, symbolic.one, symbolic.zero, symbolic.zero),
		y: vectorT(E3, symbolic.zero, symbolic.cos(θ), symbolic.sin(θ)),
		z: vectorT(E3, symbolic.zero, symbolic.sin(θ).neg(), symbolic.cos(θ))
	});

	function vecSubs(v: vector<string, symbolic>, subs: Record<string, symbolic>) {
		return v.create(...Object.values(v).map(s => s.substitute(subs)));
	}

	function matSubs(m: mat<vector<string, symbolic>, string>, subs: Record<string, symbolic>) {
		return m.create(...Object.values(m).map(c => vecSubs(c, subs)));
	}

	const rX1 = matSubs(rX as mat<vector<string, symbolic>, string>, { θ: symbolic.sym('a') });
	const rY1 = matSubs(rY as mat<vector<string, symbolic>, string>, { θ: symbolic.sym('b') });
	const rZ1 = matSubs(rZ as mat<vector<string, symbolic>, string>, { θ: symbolic.sym('c') });
	const rZYX = rZ1.matmul(rY1).matmul(rX1);

	const dd = rZYX.det();
	const rules = trigRules.concat(invTrigRules).concat(generalRules);


	let counts = 0;
	const validate = (node: symbolic, rep: symbolic) => {
		counts++;
		return real.approx(node.evaluate({a:0.3, b:0.5, c:1.2}), rep.evaluate({a:0.3,b:0.5,c:1.2}));
	};


	const out = applyRulesEgraph(dd, rules, {verbose: false, maxRounds: 6, maxExpansions: 0});
	console.log(String(out));

});

test('symbolic inverse 2x2, 3x3 and 5x5', () => {

	const rules	= trigRules.concat(invTrigRules).concat(generalRules);
	const opts: EGraphOptions	= { maxRounds: 8, maxExpansions: 16};//, debugNode: 'replace' };

	const names = 'abcdefghijklmnopqrstuvwxyz'.slice(0, 25).split('');
	const syms 	= Object.fromEntries(names.map(n => [n, symbolic.sym(n)]));

	function makeMat(c: readonly string[], r: readonly string[]) {
		let i = 0;
		return mat(
			Object.fromEntries(c.map(c => {
				return [c, vectorT(r, ...r.map(rowName => symbolic.sym(names[i++])))];
			}))
		);
	}

	function setVars0<C extends string, R extends string>(m: mat<vector<R>, C>) {
		const n = m.columns()[0]._values.length;
		return Object.fromEntries(m.columns().map((c, i) => c._values.map((v, j) => [names[i * n + j], v])).flat());
	}

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	function setVars<C extends string, R extends string>(m: mat<any, C>) {
		const n = m.columns()[0]._values.length;
		return Object.fromEntries(m.columns().map((c, i) => c._values.map((v: unknown, j: number) => [names[i * n + j], v])).flat());
	}

	function checkApprox<C extends string, R extends string>(m: mat<vector<R, symbolic>, C>, vars: Record<string, number>) {
		const cols = m.columns();
		for (let j = 0; j < cols.length; ++j) {
			const col = cols[j];
			const values = col._values;
			for (let i = 0; i < values.length; ++i) {
				const entry = values[i].evaluate(vars);
				const expected = i === j ? 1 : 0;
				if (!real.approx(entry, expected, 1e-8)) {
					const entry2 = values[i].evaluate(vars);
					throw new Error(`inverse mismatch at (${i},${j}): ${String(entry)}`);
				}
			}
		}
	}

	// 2x2
	console.log('\n2x2');
	const n2 = float2x2(
		float2(2, 5),
		float2(3, 7)
	);
	const m2 = makeMat(E2, E2);
	const d2 = m2.det();
	console.log(String(m2));
	console.log('det:', String(d2));
	const d2b = applyRulesEgraph(d2, rules, opts)
	console.log('det:', String(d2b));

	const i2 = m2.inverse();
	const p2 = m2.matmul(i2);
	console.log('2x2 inv check:'); console.log(String(p2));
	checkApprox(p2, setVars(n2));

	//const simp = startSimplify(opts);
	//const p2b = matVisit(p2, node => simplify(simp, node, rules, opts));
	const p2b = matVisit(p2, node => applyRulesEgraph(node, rules, opts));
	console.log('simplified 2x2 inv:'); console.log(String(p2b));

	assert(
		p2b.x.x === symbolic.one && p2b.x.y === symbolic.zero
	&&	p2b.y.x === symbolic.zero && p2b.y.y === symbolic.one
	);

	// 3x3
	console.log('\n3x3');
	const n3 = float3x3(
		float3(2, 7, 17),
		float3(3, 11, 19),
		float3(5, 13, 23)
	);

	const m3 = makeMat(E3, E3);
	const d3 = m3.det();
	console.log(String(m3));
	console.log('det:', String(d3));
	const d3b = applyRulesEgraph(d3, rules, opts)
	console.log('det:', String(d3b));
	const i3 = m3.inverse();
	const p3 = m3.matmul(i3);
//	const p3b = matVisit(p3, node => simplify(simp, node, rules, opts));
	
	checkApprox(p3, setVars(n3));
	
	// 4x4
	console.log('\n4x4');
	const n4 = float4x4(
		float4(2, 7, 17, 23),
		float4(3, 11, 19, 29),
		float4(5, 13, 23, 31),
		float4(7, 17, 29, 37)
	);

	const m4 = makeMat(E4, E4);
	const d4 = m4.det();
	console.log(String(m4));
	console.log('det:', String(d4));
//	const d4b = applyRulesEgraph(d4, rules, opts)
//	console.log('det:', String(d4b));
	const i4 = m4.inverse();
	const p4 = m4.matmul(i4);
//	const p4b = matVisit(p4, node => applyRulesEgraph(node, rules, opts));
	
	checkApprox(p4, setVars(n4));
	
	// 5x5
	console.log('\n5x5');
	const n5 = mat({
		x: vector(E5, 1,0,0,0,0),
		y: vector(E5, 2,2,0,0,0),
		z: vector(E5, 3,1,3,0,0),
		w: vector(E5, 4,0,1,4,0),
		c5: vector(E5, 5,1,2,1,5)
	});

	const m5 = makeMat(E5, E5);
	const d5 = m5.det();
	console.log('5x5 det:', String(d5));
	const i5 = m5.inverse();
//	const i5b = matVisit(i5, node => applyRulesEgraph(node, rules, opts));
	const p5 = m5.matmul(i5);
//	const p5b = matVisit(p5, node => applyRulesEgraph(node, rules, opts));
	checkApprox(p5, setVars(n5));

});


test('symbolic polynomials', () => {
	let poly = Polynomial<symbolic>([symbolic.from(1)]);
	const bindings = {
		A: complex(1),
		B: complex(2),
		C: complex(3),
		D: complex(4),
		E: complex(5),
		F: complex(6),
		G: complex(7),
		H: complex(8),
	};
 
	function validate(a: symbolic, b: symbolic) {
		const va = a.evaluateT(complex, bindings);
		const vb = b.evaluateT(complex, bindings);
		if (va && vb && (va.approx(vb, 1e-10) || va.approx(vb.neg(), 1e-10)))
			return true;
		const va2 = a.evaluateT(complex, bindings);
		const vb2 = b.evaluateT(complex, bindings);
			return true;
		//return false;
	}

	for (let j = 1; j < 4; ++j) {
		poly = poly.mul(Polynomial<symbolic>([symbolic.sym('ABCDEFGH'[j  - 1]).neg(), symbolic.from(1)]));
		console.log(`degree ${j} polynomial: ${poly.toString()}`);
		const roots = poly.realRoots();
		console.log(roots.map(String).join('\n'));
		console.log('simplifying:');
		for (const r of roots) {
			const timeoutMs = 20000;
			const start = Date.now();
			const callback = () => {
				if (Date.now() - start > timeoutMs) {
					console.warn(`egraph-diagnostic: timeout of ${timeoutMs}ms`);
					return false;
				}
				return true;
			};

			const r2 = applyRulesEgraph(r, generalRules, {verbose: false, debugNode: 'replace', /*callback, */validate});
			console.log(String(r2));
		}
	}
});
