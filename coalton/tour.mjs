export const escapeHTML = text => String(text).replace(/[&<>"']/gu, char =>
  ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[char]));
const headingID = text => text.toLowerCase().replace(/[^\p{L}\p{N}\s-]/gu, '').trim().replace(/\s+/gu, '-');

function transcriptInput(source) {
  const lines = source.split('\n'), forms = [];
  for (let index = 0; index < lines.length; index++) {
    const prompt = lines[index].match(/^(?:([A-Z][A-Z0-9_/-]*)|)>\s*(.*)$/u);
    if (!prompt) continue;
    let form = prompt[2], depth = 0, string = false, escaped = false, complete = false;
    const scan = text => {
      let comment = false;
      for (const char of text) {
        if (comment) {if (char === '\n') comment = false; else continue;}
        if (string) {
          if (escaped) escaped = false;
          else if (char === '\\') escaped = true;
          else if (char === '"') string = false;
        } else if (char === ';') comment = true;
        else if (char === '"') string = true;
        else if (char === '(') depth++;
        else if (char === ')') {depth--; if (!depth) complete = true;}
      }
    };
    scan(form + '\n');
    while (!complete && index + 1 < lines.length) {form += '\n' + lines[++index]; scan(lines[index] + '\n');}
    if (prompt[1] && prompt[1] !== 'COALTON-USER') forms.push(`(cl:in-package :${prompt[1]})`);
    forms.push(form);
  }
  return forms.join('\n\n');
}

export function parseTour(markdown) {
  markdown = markdown.replace(/^---\n[\s\S]*?\n---\n/u, '').replace('{{<toc>}}', '');
  const parts = [], cells = []; let offset = 0, section = 'Introduction';
  const heading = text => {for (const match of text.matchAll(/^#{2,6}\s+(.+)$/gmu)) section = match[1];};
  for (const match of markdown.matchAll(/^```([^\n]*)\n([\s\S]*?)^```[^\n]*$/gmu)) {
    const prose = markdown.slice(offset, match.index); heading(prose); parts.push({kind: 'prose', text: prose});
    const language = match[1].trim(), original = match[2].trimEnd();
    const transcript = /^(?:[A-Z][A-Z0-9_/-]*|)>\s*\(/mu.test(original);
    const editable = language === 'lisp' || transcript || /^\s*\(/u.test(original);
    if (editable) {
      const code = transcript ? transcriptInput(original) : original;
      const first = code.replace(/^\s*;[^\n]*\n/gmu, '').trimStart();
      const head = first.match(/^\(\s*([^\s()]+)/u)?.[1]?.toLowerCase();
      const mode = /^(?:declare|define(?:-.*)?|derive|repr|specialize)$/u.test(head ?? '') ? 'definitions'
        : ['let', 'let*', 'rec', 'for', 'for*', 'nest', 'pipe', 'lisp', 'progn', 'do', 'dynamic-bind'].includes(head) ? 'expression' : 'lisp';
      const cell = {id: `example-${cells.length + 1}`, section, original, code, mode, transcript,
        sketch: /\.\.\.|<[^>\n]+>|\[label\]/u.test(code)};
      cells.push(cell); parts.push({kind: 'cell', cell});
    } else parts.push({kind: 'output', text: original});
    offset = match.index + match[0].length;
  }
  parts.push({kind: 'prose', text: markdown.slice(offset)});
  return {parts, cells};
}

const inline = text => text.split(/(`[^`]+`|\[[^\]]+\]\([^\s)]+\))/gu).map(part => {
  if (part.startsWith('`')) return `<code>${escapeHTML(part.slice(1, -1))}</code>`;
  const link = part.match(/^\[([^\]]+)\]\(([^)]+)\)$/u);
  if (link) {
    const url = /^(?:https?:\/\/|#)/u.test(link[2]) ? link[2]
      : `https://github.com/coalton-lang/coalton/blob/main/docs/manual/site/topics/${link[2]}`;
    return `<a href="${escapeHTML(url)}">${escapeHTML(link[1])}</a>`;
  }
  return escapeHTML(part).replace(/\*\*([^*]+)\*\*/gu, '<strong>$1</strong>').replace(/\*([^*]+)\*/gu, '<em>$1</em>');
}).join('');

function proseHTML(text) {
  return text.trim().split(/\n\s*\n/u).filter(Boolean).map(block => {
    const heading = block.match(/^(#{2,6})\s+(.+)$/u);
    if (heading) return `<h${heading[1].length} id="${escapeHTML(headingID(heading[2]))}">${inline(heading[2])}</h${heading[1].length}>`;
    if (/^>/u.test(block)) return `<aside class="note">${inline(block.replace(/^>\s*/gmu, '').replace(/^\[!\w+\]\n/u, ''))}</aside>`;
    if (/^(?:[-*]|\d+\.) /u.test(block)) return '<ul>' + block.split('\n').map(line => `<li>${inline(line.replace(/^(?:[-*]|\d+\.)\s*/u, ''))}</li>`).join('') + '</ul>';
    return `<p>${inline(block.replaceAll('\n', ' '))}</p>`;
  }).join('\n');
}

export function renderTour(tour) {
  return tour.parts.map(part => {
    if (part.kind === 'prose') return proseHTML(part.text);
    if (part.kind === 'output') return `<pre class="tour-output">${escapeHTML(part.text)}</pre>`;
    const cell = part.cell;
    return `<section class="cell" id="${cell.id}" aria-label="${escapeHTML(cell.section)} example">
      <div class="cell-heading"><a href="#${cell.id}">${cell.id.replace('example-', 'Example ')}</a><span>${escapeHTML(cell.section)}</span></div>
      ${cell.sketch ? '<p class="cell-note">Syntax sketch: fill in the placeholders before evaluating.</p>' : ''}
      <label class="sr-only" for="${cell.id}-source">Source for ${cell.id}</label>
      <textarea id="${cell.id}-source" spellcheck="false" autocapitalize="off" autocomplete="off" rows="${Math.min(20, Math.max(3, cell.code.split('\n').length + 1))}">${escapeHTML(cell.code)}</textarea>
      <div class="cell-actions"><button class="run" disabled>Evaluate</button><button class="restore" type="button">Restore example</button>
      <label>Input <select class="mode">${[['lisp', 'Lisp forms'], ['expression', 'Coalton expression'], ['definitions', 'Coalton definitions']].map(([mode, title]) => `<option value="${mode}"${cell.mode === mode ? ' selected' : ''}>${title}</option>`).join('')}</select></label><span class="timing"></span></div>
      ${cell.transcript ? `<details><summary>Original tour transcript</summary><pre>${escapeHTML(cell.original)}</pre></details>` : ''}
      <pre class="result" aria-live="polite" hidden></pre>
    </section>`;
  }).join('\n');
}
