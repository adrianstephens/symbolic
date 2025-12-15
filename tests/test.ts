export interface Equal<T> {
	eq(b: T): boolean;
}
type Testable<T> = T extends (string | number | boolean | null | undefined) 
	? T 
	: T & Equal<T>;

export function expect<T>(v: Testable<T>, description?: string) {
	return {
		toEqual(v2: T) {
			const success = typeof v === 'object' && v ? v.eq(v2) : v === v2;
			console.log(`${success ? '✓' : '✗'}: ${description ? description + ' ' : ''}${v} === ${v2}`);
			//if (!success)
			//	console.log(`fail: expected ${v2}, got ${v}`);
		},
		toBeCloseTo(v2: number, tol = 1e-8) {
			const success = typeof v === 'number' && Math.abs(v - v2) <= tol;
			console.log(`${success ? '✓' : '✗'}: ${description ? description + ' ' : ''}${v} ≈ ${v2}`);
		},
		check(test: (v: T) => boolean) {
			const success = test(v);
			console.log(`${success ? '✓' : '✗'}: ${description ?? ''}`);
		},
	};
}

export function test(name: string, fn: ()=>void) {
	console.log('---------------------');
	console.log("testing: " + name);
	fn();
	console.log("finished: " + name);
}

export function verify<T>(v1: T, v2: T, test: (v1: T, v2: T) => boolean, description?: string) {
	const success = test(v1, v2);
	console.log(`${success ? '✓' : '✗'}: ${description ? description + ' ' : ''}expected ${v2}, got ${v1}`);
}

export function assert(condition: boolean, msg?: string) {
	if (!condition)
		throw new Error(msg || 'assertion failed');
}


export function approxArray(a: number[], b: number[], tol = 1e-9) {
    if (a.length !== b.length)
        return false;
    for (let i = 0; i < a.length; ++i) {
        if (Math.abs(a[i] - b[i]) > tol)
            return false;
    }
    return true;
}

export function makeApproxArray(tol: number) {
    return (a: number[], b: number[]) => approxArray(a, b, tol);
}

export function sequence(length: number, from = 0, step = 1) {
	return Array.from({ length }, (_, k) => from + k * step);
}
