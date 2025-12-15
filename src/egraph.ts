import { symbolic } from './symbolic';
import { Rule, Scorer, scoreFactory } from './symbolicRules';


const EGraphOptionsDefault = {
	scorer:			scoreFactory(),
	maxExpansions:	16,
	verbose:		false,
	showObjects:	false,
};

export type EGraphOptions0 = typeof EGraphOptionsDefault & {
	validate?:		(node: symbolic, rep: symbolic) => boolean,
	callback?:		() => boolean;
}

export interface EGraphOptions extends Partial<EGraphOptions0> {
	maxRounds?:		number,
	verbose?:		boolean,
	debugNode?: 	string,
} 

class EClass extends symbolic {
	parent?: EClass;
	parents = new Set<symbolic>();
	bestNode?: symbolic;
	bestScore?: number;
	
	constructor(id: string, public nodes: Set<symbolic>) {
		super(id);
	}

	find(): EClass {
		if (!this.parent)
			return this;
		const r = this.parent.find();
		this.parent = r;
		return r;
	}

	updateBestNode(scorer: Scorer) {
		let best: symbolic | undefined;
		let bestScore = Infinity;
		
		for (const node of this.nodes) {
			const score = scorer(node, bestScore);
			if (score < bestScore) {
				bestScore = score;
				best = node;
			}
		}
		
		this.bestNode = best;
		this.bestScore = bestScore;
	}

	repr(): symbolic|undefined {
		if (!this.bestNode) {
			const it = this.nodes.values();
			this.bestNode = it.next().value;
		}
		return this.bestNode;
	}
	_toString(): string { return `EClass ${this.id}`; }
}

export default class EGraph {
	private enodeMap		= new Map<string, EClass>();
	private canonicalMap	= new Map<string, EClass>();
	public created: string[] = [];

	constructor(public opts: EGraphOptions0 = EGraphOptionsDefault) {
	}

	private maybeObject(o: object) {
		return this.opts.showObjects ? o : '';
	}

	/*private*/ eclassOfNode(node: symbolic): EClass | undefined {
		return this.enodeMap.get(node.id)?.find();
	}

	private computeCanonicalSig(node: symbolic): string {
		const mapped = node.visit({post: (n: symbolic) =>
			this.eclassOfNode(n) ?? n
		});
		return mapped.id;
	}

	// Collect parent nodes that directly reference any node in this class
	private getParents(ec: EClass): Set<symbolic> {
		const parents = new Set<symbolic>();
		for (const childNode of ec.nodes) {
			const childEClass = this.enodeMap.get(childNode.id);
			if (childEClass)
				for (const p of childEClass.parents)
					parents.add(p);
		}
		return parents;
	}
	
	addSymbolic(node: symbolic): EClass {
		const existing = this.eclassOfNode(node);
		if (existing)
			return existing;

		// Post-order traversal: children are processed before parents so their eclasses exist when we handle a parent.
		// For each visited node we compute a canonical signature (by substituting immediate children with their current eclass reps), create or reuse an EClass
		node.visit({
			noRemake: true,
			pre: (n: symbolic, parent?: symbolic) => {
				const eclass = this.eclassOfNode(n);
				if (eclass) {
					if (parent) {
						// record that `parent` directly references this node
						eclass.parents.add(parent);
					}
					return undefined;
				}
				return n;
			},
			post: (n: symbolic, parent?: symbolic) => {
				// If this node already has an eclass (possibly created earlier), skip
				if (this.eclassOfNode(n))
					return undefined;

				if (this.opts.verbose)
					console.log(`egraph-diagnostic: adding node ${n}`, this.maybeObject(n));

				const canon = this.computeCanonicalSig(n);
				const eclass = new EClass(`ec:${this.canonicalMap.size}`, new Set);
				//const eclass = new EClass('ec:' + canon, new Set);
				eclass.nodes.add(n);
				this.enodeMap.set(n.id, eclass);

				this.canonicalMap.set(canon, eclass);

				this.created.push(n.id);

				// attach this eclass as a child of its parent (if any)
				if (parent)
					eclass.parents.add(parent);
				return;
			}
		});

		const out = this.eclassOfNode(node);
		if (!out)
			throw new Error(`addSymbolic: failed to register node ${node}`);
		return out;
	}

	// merge with congruence-closure worklist processing implemented iteratively
	private mergeClasses(a: EClass, b: EClass): EClass | null {
		if (a === b)
			return null;

		// worklist of pairs to merge
		const work: [EClass, EClass][] = [[a.find(), b.find()]];
		let result: EClass | null = null;

		while (work.length > 0) {
			let [x, y] = work.pop()!;
			x = x.find();
			y = y.find();
			if (x === y)
				continue;

			if (this.opts.verbose)
				console.log(`egraph-diagnostic: merging ${x} with ${y}`);

			if (this.opts.validate && !this.opts.validate(x.repr()!, y.repr()!))
				throw new Error("Validation failed during merge");

			// Merge smaller class into larger class (by node count).
			if (x.nodes.size < y.nodes.size)
				[x, y] = [y, x];

			// Collect parents that reference nodes in y (before we move them)
			const parentsToRecompute = this.getParents(y);

			// attach source to target and move nodes
			y.parent = x;
			for (const n of y.nodes)
				x.nodes.add(n);
			y.nodes.clear();

			if (y.bestScore && (!x.bestScore || y.bestScore < x.bestScore)) {
				x.bestScore = y.bestScore;
				x.bestNode = y.bestNode;
			}

			// Recompute parent identifiers only for parents that referenced nodes from the attached class `y`.
			// Compute the canonical signature by substituting immediate children with their current eclass representatives.
			// This detects collisions where a parent (with substituted children) matches an existing enode and therefore must be merged.
			for (const parent of parentsToRecompute) {
				const newSig			= this.computeCanonicalSig(parent);
				const parentEClass		= this.eclassOfNode(parent)!;
				const existingEClass	= this.canonicalMap.get(newSig);
				if (existingEClass) {
					work.push([existingEClass, parentEClass]);
				} else  {
					this.canonicalMap.set(newSig, parentEClass);
				}
			}

			if (this.opts.validate) {
				const repr = x.repr()!;
				for (const n of x.nodes) {
					if (!this.opts.validate(n, repr))
						throw new Error("Validation failed during merge");
				}
			}

			// record the merged representative
			result = x;

		}

		return result;
	}

	// Materialize small Cartesian products of child alternatives so parent combinations become explicit enodes.
	// This enables parent-level rules (e.g. mul-distribute) to fire on combinations of child alternatives.
	private addProducts(parent: symbolic): void {
		const orderedChildEcs: EClass[] = [];
		parent.visit({
			noRemake: true,
			post: (child: symbolic, parent2?: symbolic) => {
				if (parent === parent2) {
					const ce = this.eclassOfNode(child);
					if (ce)
						orderedChildEcs.push(ce.find());
				}
				return child;
			}
		});
		
		// Update best nodes for each child eclass
		//for (const ec of orderedChildEcs)
		//	ec.updateBestNode(this.scorer);
		
		// Strategy: Only expand combinations involving the best node from each eclass
		// This prevents combinatorial explosion while ensuring good candidates are tried
		const childCandidates = orderedChildEcs.map(ec => {
			const nodes = Array.from(ec.nodes);
			if (nodes.length === 1)
				return nodes;
			// For eclasses with multiple nodes, include the best one plus originals
			//const best = ec.bestNode!;
			//const originals = nodes.slice(0, Math.min(2, nodes.length)); // Keep first 2 as originals
			//return originals.includes(best) ? originals : [best, ...originals];
			return nodes;
		});
		
		const total = childCandidates.reduce((acc, c) => acc * c.length, 1);

		if (total > 1 && total < this.opts.maxExpansions) {
			for (let j = 0; j < total; j++) {
				let i = j;
				const selection = childCandidates.map((candidates: symbolic[]) => {
					const mod = i % candidates.length;
					i = Math.floor(i / candidates.length);
					return candidates[mod];
				});

				i = 0;
				const concrete = parent.visit({
					pre: (n: symbolic, parent2?: symbolic) => parent === parent2 ? selection[i++] : n
				});

				if (this.opts.validate && !this.opts.validate(parent, concrete))
					throw new Error(`Validation failed during merging expansion: parent=${String(parent)}, concrete=${String(concrete)}`);

				const eclass = this.addSymbolic(concrete);
				this.mergeClasses(this.eclassOfNode(parent)!, eclass);
			}
		}
	}
	
	rewrite(a: symbolic, b: symbolic) {
		if (this.opts.validate && !this.opts.validate(a, b))
			throw new Error(`EGraph validation failed for ${a} => ${b}`);

		const aclass	= this.addSymbolic(a);
		const bclass	= this.addSymbolic(b);
		const result	= this.mergeClasses(aclass, bclass);
		if (result) {
			const parents = this.getParents(result);
			for (const p of parents)
				this.addProducts(p);
		}
	}

	applyRules(rules: Rule[], debugNode?: string, callback?: () => boolean): boolean {
		const keys = this.created;
		this.created = [];


		for (const nid of keys) {
			const node = symbolic.getById(nid)! as symbolic;

			for (const r of rules) {
				// attempt (allow non-exact matching so rules can bind _addrest/_mulrest
				// and match through single numeric factors)
				const bs = r.match(node);

				if (!bs || (r.guard && !r.guard(bs, this))) {
					if (debugNode === 'all' || debugNode === nid)
						console.warn(`egraph-diagnostic: rule ${r.name} on ${node} ${bs ? "matched, but guard failed" : "no match"}`);
					continue;
				}

				const rep = r.replace(bs);
				// replacement produced
				if (this.opts.verbose || debugNode === 'all' || debugNode === 'replace' || debugNode === nid)
					console.log(`egraph-diagnostic: rule ${r.name} on ${node} produced ${rep ? rep : '<none>'}:`, this.maybeObject(node), this.maybeObject(rep));

				if (this.opts.validate && !this.opts.validate(node, rep))
					throw new Error(`EGraph validation failed for rule ${r.name} on node ${node} producing ${rep}`);

				const eclass	= this.eclassOfNode(node)!;
				const existing	= this.addSymbolic(rep);
				const merged	= this.mergeClasses(eclass, existing);

				if (merged) {
					const parents = this.getParents(merged);
					for (const p of parents)
						this.addProducts(p);
				}

				if (!this.opts.verbose && merged && (debugNode === 'changed' || debugNode === 'all' || debugNode === nid))
					console.log(`egraph-diagnostic: merged ${eclass} with ${existing}`);

				// Call the callback after each rule application
				if (callback && !callback())
					return false;
			}
		}



		return this.created.length > 0;
	}

	private	findBestNode(ec: EClass, scorer: Scorer) {
		let bestScore = ec.bestScore ?? Infinity;
		let bestNode	= ec.bestNode;
		for (const node of ec.nodes) {
			const candidate = node.visit({
				pre: (n: symbolic) => {
					if (n === node)
						return n;
					const ce = this.eclassOfNode(n);
					return ce?.bestNode ?? n;
				}
			});

			const cost = scorer(candidate, bestScore);
			if (cost < bestScore) {
				bestNode = candidate;
				bestScore = cost;
			}
		}
		if (bestNode === ec.bestNode)
			return false;
		ec.bestNode = bestNode;
		ec.bestScore = bestScore;
		return true;
	}

	// Extract best representative from the equivalence class of nid using iterative DP and scorer
	extractBest(node: symbolic, scorer: (n: symbolic) => number, maxIters = 50): symbolic {
		const root = this.eclassOfNode(node);
		if (!root)
			return node;

		// Build deduplicated array of representative roots from enodeMap
		const seenRoots = new Set<EClass>();
		this.enodeMap.values().forEach((ec: EClass) => seenRoots.add(ec.find()));
		const roots: EClass[] = Array.from(seenRoots);

		// Update best nodes for all eclasses
		for (const ec of roots)
			ec.updateBestNode(scorer);

		if (this.opts.verbose) {
			for (const ec of roots)
				console.warn({rep: ec.id, bestCost: ec.bestScore, bestNode: ec.bestNode, nodes: Array.from(ec.nodes).map(this.opts.showObjects ? Object : String)});
		}

		// Iterative stabilization: rebuild candidates by substituting children with best representatives
		for (let iter = 0, changed = true; changed && iter <= maxIters; iter++) {
			for (const ec of roots) {
				const changed1 = this.findBestNode(ec, scorer);
				if (changed1 && this.opts.verbose)
					console.warn(`egraph-diagnostic: updating best for ${ec.id} to ${ec.bestScore} node: ${ec.bestNode}`, this.maybeObject(ec.bestNode!));
				changed ||= changed1;
			}
		}

		if (this.opts.verbose) {
			console.warn('egraph-diagnostic: final best choices:');
			for (const ec of roots)
				console.warn(`  rep=${ec.id} cost=${ec.bestScore} node=${ec.bestNode}`);
		}
		
		return root.bestNode!;
	}
}


export function applyRulesEgraph(node: symbolic, rules: Rule[], opts?: EGraphOptions): symbolic {
	const egraph = new EGraph({...EGraphOptionsDefault, ...opts});
	if (opts?.verbose)
		console.log('applyRulesEgraph', String(node));

	egraph.addSymbolic(node);

	const rounds = opts?.maxRounds ?? 6;
	for (let i = 0; i < rounds; i++) {
		if (opts?.verbose)
			console.warn('egraph-diagnostic: round', i + 1);
		if (!egraph.applyRules(rules, opts?.debugNode, opts?.callback))
			break;
	}

	return egraph.extractBest(node, opts?.scorer ?? scoreFactory());
}

export function startSimplify(opts?: EGraphOptions) {
	return  new EGraph({...EGraphOptionsDefault, ...opts});
}

export function simplify(egraph: EGraph, node: symbolic, rules: Rule[], opts?: EGraphOptions) {
	egraph.addSymbolic(node);

	const rounds = opts?.maxRounds ?? 6;
	for (let i = 0; i < rounds; i++) {
		if (!egraph.applyRules(rules, opts?.debugNode, opts?.callback))
			break;
	}

	return egraph.extractBest(node, opts?.scorer ?? scoreFactory());
}
