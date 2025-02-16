# scryer

This is a Typescript package that embeds [Scryer Prolog](https://github.com/mthom/scryer-prolog).

Currently on @bakaq's [PR branch](https://github.com/bakaq/scryer-prolog/tree/wasm_rework).

Experimental, API will change.

```
npm install scryer
```

```typescript
import { init, Prolog } from "scryer";

await init();

const pl = new Prolog();
const query = pl.query("X = 1 ; X = 2.");
for (const answer of query) {
	console.log(answer.bindings);
}
```
