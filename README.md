# scryer

This is a Typescript package that embeds [Scryer Prolog](https://github.com/mthom/scryer-prolog).

Experimental, API will change. Currently it's (roughly) based on [trealla-js](https://github.com/guregu/trealla-js)'s API.

```
npm install scryer
```

```typescript
import { Prolog } from "scryer";

const pl = new Prolog();
const query = pl.query("X = 1 ; X = 2.");
for (const answer of query) {
	console.log(answer.bindings);
}
```

For browsers, you can use [esm.sh](https://esm.sh) or other CDNs to import it directly:

```html
<script type="module">
	import { Prolog } from "https://esm.sh/scryer@0.5.0"; // make sure to use the latest version instead :-)
	// query stuff
</script>
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
