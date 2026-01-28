/* eslint-disable @typescript-eslint/no-explicit-any */

export interface Equal<T> {
	eq(b: T): boolean;
	approx?(b: T, tol: number): boolean;
}
type Testable<T> = T extends (string | number | boolean | null | undefined) 
	? T 
	: T & Equal<T>;

function output(success: boolean, description: string) {
	if (success)
		console.log(`✓: ${description}`);
	else
		console.error(`✗: ${description}`);
}

export function assert(condition: boolean, msg?: string): asserts condition {
	if (!condition)
		throw new Error(msg || 'assertion failed');
}

export function verify<T>(v1: T, v2: T, test: (v1: T, v2: T) => boolean, description?: string) {
	output(test(v1, v2), `${description ? description + ' ' : ''}expected ${v2}, got ${v1}`);
}

export function expect<T>(v: Testable<T>, description?: string) {
	return {
		toEqual(v2: T) {
			output(typeof v === 'object' && v ? v.eq(v2) : v === v2, `${description ? description + ' ' : ''}${v} = ${v2}`);
		},
		toBeCloseTo(v2: number, tol = 1e-8) {
			output(approx(v as any, v2, tol), `${description ? description + ' ' : ''}${v} ≈ ${v2}`);
		},
		check(test: (v: T) => boolean) {
			output(test(v), description ?? '');
		},
	};
}

export function test(name: string, fn: ()=>void) {
	console.log(`---------------------\ntesting: ${name}`);
	//try {
		fn();
	//} catch (e: unknown) {
	//	console.error(`failed: ${name} with ${e}`);
	//	return;
	//}
	console.log("finished: " + name);
}

function approx<T extends Equal<T>|number>(a: T, b: T, tol = 1e-8) {
	return typeof a === 'object' && a ? a.approx?.(b, tol) ?? false : Math.abs((a as number) - (b as number)) <= tol;
}

export function approxArray<T extends Equal<T>|number>(a: T[], b: T[], tol = 1e-9) {
    if (a.length !== b.length)
        return false;
    for (let i = 0; i < a.length; ++i) {
        if (!approx(a[i], b[i], tol))
            return false;
    }
    return true;
}

export function makeApproxArray(tol: number) {
    return <T extends Equal<T>|number>(a: T[], b: T[]) => approxArray(a, b, tol);
}

