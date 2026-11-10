import fs from 'node:fs/promises';
import assert from 'node:assert/strict';
import {parseTour, renderTour} from '../coalton/tour.mjs';
import {coaltonInput} from '../scripts/coalton.mjs';
const markdown = await fs.readFile('vendor/coalton/docs/manual/site/topics/whirlwind-tour.md', 'utf8');
const tour = parseTour(markdown), html = renderTour(tour);
const fences = [...markdown.matchAll(/^```[^\n]*\n[\s\S]*?^```[^\n]*$/gmu)].length;
assert.equal(tour.parts.filter(part => part.kind !== 'prose').length, fences);
assert.equal(tour.cells.length, 139);
assert.equal((html.match(/<textarea /gu) ?? []).length, tour.cells.length);
assert.equal((html.match(/class="run"/gu) ?? []).length, tour.cells.length);
assert.ok(tour.cells.every(cell => cell.code.trim().length));
assert.ok(tour.cells.some(cell => cell.sketch));
assert.ok(tour.cells.some(cell => cell.transcript));
assert.ok(!html.includes('{{<toc>}}'));
assert.ok(!html.includes('hideMeta:'));
assert.match(html, /id="packages"/u);
const fixture = parseTour(`## Example

<script>globalThis.injected=true</script>

\`\`\`lisp
COALTON-USER> (coalton
  (+ 20 22))
42
COALTON-USER> (coalton "(text)")
"(text)"
\`\`\`
`);
assert.equal(fixture.cells[0].code, '(coalton\n  (+ 20 22))\n\n(coalton "(text)")');
const rendered = renderTour(fixture);
assert.ok(!rendered.includes('<script>'));
assert.match(rendered, /&lt;script&gt;/u);
assert.match(rendered, /Original tour transcript/u);
assert.ok(coaltonInput('(define x 42)', 'definitions').includes('(coalton:coalton-toplevel\n(define x 42)\n)'));
assert.ok(coaltonInput('(+ 20 22)', 'expression').includes('(coalton:coalton\n(+ 20 22)\n)'));
console.log(`Coalton tour: all ${fences} fences retained, ${tour.cells.length} editable cells, transcript extraction, modes, links and HTML escaping passed.`);
