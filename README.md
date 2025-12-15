# @isopodlabs/symbolic

A symbolic mathematics and computer algebra system (CAS) for TypeScript.

## Features

- **Symbolic differentiation** - Automatic differentiation of expressions
- **Symbolic integration** - Comprehensive integration with multiple techniques:
  - Power rule
  - U-substitution (chain rule detection)
  - Integration by parts (LIATE heuristic)
  - Trig/hyperbolic power reduction formulas
  - Trig × Exp products
  - Product-to-sum identities
  - Pattern matching for inverse trig/hyperbolic derivatives
- **E-graph simplification** - Equality saturation for expression simplification
- **Pattern matching** - Flexible pattern matching with bindings
- **Expression interning** - Efficient expression equality via structural sharing

## Installation

```bash
npm install @isopodlabs/symbolic
```

## Usage

```typescript
import { symbolic, parse } from '@isopodlabs/symbolic';

const x = symbolic.variable('x');

// Differentiation
const f = symbolic.sin(x.pow(2));
const df = f.derivative('x');
console.log(df.toString()); // 2(cos(x^2) * x)

// Integration
const g = x.mul(symbolic.exp(x));
const integral = g.integral('x');
console.log(integral.toString()); // exp(x) * x - exp(x)

// Parsing
const expr = parse(symbolic, 'sin(x)^2 + cos(x)^2');
console.log(expr.toString());

// Simplification with e-graph
import { EGraph } from '@isopodlabs/symbolic';
const egraph = new EGraph(expr);
const simplified = egraph.simplify();
```

## Supported Functions

- **Trig**: sin, cos, tan, asin, acos, atan, atan2
- **Hyperbolic**: sinh, cosh, tanh, asinh, acosh, atanh
- **Exponential/Log**: exp, log, pow
- **Other**: abs, sqrt (via rpow)

## API

### `symbolic`

The main class for symbolic expressions.

```typescript
// Creation
symbolic.from(n: number | rational): symbolic
symbolic.variable(name: string): symbolic
symbolic.sym(name: string): symbolic  // alias for variable

// Functions
symbolic.sin(x), symbolic.cos(x), symbolic.tan(x)
symbolic.sinh(x), symbolic.cosh(x), symbolic.tanh(x)
symbolic.asin(x), symbolic.acos(x), symbolic.atan(x)
symbolic.asinh(x), symbolic.acosh(x), symbolic.atanh(x)
symbolic.exp(x), symbolic.log(x), symbolic.pow(base, exp)
symbolic.atan2(y, x)

// Operations
expr.add(other), expr.sub(other), expr.mul(other), expr.div(other)
expr.neg(), expr.pow(n), expr.npow(n: number), expr.rpow(num, den)

// Calculus
expr.derivative(variable: string): symbolic
expr.integral(variable: string): symbolic

// Transformation
expr.expand(opts?): symbolic
expr.substitute(bindings): symbolic
expr.visit(visitor): symbolic

// Evaluation
expr.evaluate(env: Record<string, number>): number
expr.toString(): string
```

### `EGraph`

E-graph based simplification.

```typescript
const egraph = new EGraph(expr, options?);
const simplified = egraph.simplify();
```

### `parse`

Parse string expressions.

```typescript
const expr = parse(symbolic, 'x^2 + 2*x + 1');
```

## License

MIT
