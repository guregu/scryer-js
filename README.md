# scryer

This is a Typescript package that embeds [Scryer Prolog](https://github.com/mthom/scryer-prolog).

Experimental, API will change. Currently it's (roughly) based on [trealla-js](https://github.com/guregu/trealla-js)'s API.

```
npm install scryer
```

```typescript
import { init, Prolog } from "scryer";

await init();

const kb1 = `
:- use_module(library(format)).
:- use_module(library(clpz)).
:- use_module(library(lists)).

sudoku(Rows) :-
length(Rows, 9), maplist(same_length(Rows), Rows),
append(Rows, Vs), Vs ins 1..9,
maplist(all_distinct, Rows),
transpose(Rows, Columns),
maplist(all_distinct, Columns),
Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
blocks(As, Bs, Cs),
blocks(Ds, Es, Fs),
blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
blocks(Ns1, Ns2, Ns3).
`;

const kb2 = `
problem(1, [[_,_,_,_,_,_,_,_,_],
			[_,_,_,_,_,3,_,8,5],
			[_,_,1,_,2,_,_,_,_],
			[_,_,_,5,_,7,_,_,_],
			[_,_,4,_,_,_,1,_,_],
			[_,9,_,_,_,_,_,_,_],
			[5,_,_,_,_,_,_,7,3],
			[_,_,2,_,1,_,_,_,_],
			[_,_,_,_,4,_,_,_,9]]).
`;

const pl = new Prolog();
pl.consultText(kb1);
pl.consultText(kb2);

const query = pl.query("problem(1, Rows), sudoku(Rows), maplist(portray_clause, Rows).");
for (const answer of query) {
	console.log(answer.bindings);
}
```

For browsers, you can use [esm.sh](https://esm.sh) or other CDNs to import it directly:

```html
<script type="module">
	import { init, Prolog } from "https://esm.sh/scryer"; // ideally add version info to the URL, e.g. scryer@0.1.0
	await init();
	// query stuff
</script>
```
## Development

After cloning the repository, make sure to initialize and update the submodules:

```bash
git submodule update --init --recursive
```
Alternatively, you can do this directly during the clone:

```bash
git clone --recurse-submodules -j8 <repository-url>
```
make sure to have [wasm-pack](https://rustwasm.github.io/wasm-pack/installer/) installed.

Once the submodules are in place and wasm-pack is installed, run the following commands:

```bash
npm install
npm run compile    # Installs the WASM package of Scryer Prolog and other dependencies
npm run build      # Transpiles TypeScript code
```

## Usage

### Binding variables

```typescript
const answer = pl.queryOnce("X is Y * 2.", { bind: { Y: 21 } });
console.log(answer.bindings); // { X: 42, Y: 21 }
```

### Template strings

The `prolog` string template literal is an easy way to escape terms.
Each `${value}` will be interpreted as a Prolog term.

```typescript
import { prolog, Variable } from "scryer";
const answer = pl.queryOnce(
	prolog`atom_chars(${new Variable("X")}, ${"Hello!"}).`,
);
console.log(answer.bindings); // { X: Atom('Hello!') }
```
