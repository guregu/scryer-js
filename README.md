# scryer

This is a Typescript package that embeds [Scryer Prolog](https://github.com/mthom/scryer-prolog).

Experimental, API will change. Currently it's (roughly) based on [trealla-js](https://github.com/guregu/trealla-js)'s API.

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

For browsers, you can use [esm.sh](https://esm.sh) or other CDNs to import it directly:

```html
<script type="module">
  import { init, Prolog } from "https://esm.sh/scryer"; // ideally add version info to the URL, e.g. scryer@0.1.0
  await init();
  // query stuff
</script>
```
