import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import assert from 'node:assert/strict';
import {collectCompilerReport,renderCompilerReport} from '../scripts/build-compiler-report.mjs';
const root=fs.mkdtempSync(path.join(os.tmpdir(),'miricl-compiler-report-'));
try{
 const base={schemaVersion:1,kind:'compiler-statistics',commit:'current',runId:'17',status:'complete',phase:'native',job:'applications'};
 const records=[
  {...base,id:'a',workload:'alpha',counters:[{rule:'optimization/inline-primitive',detail:'car-cons',count:3}]},
  {...base,id:'b',workload:'beta',status:'partial',failure:'<script>bad</script>',counters:[{rule:'optimization/inline-primitive',detail:'car-cons',count:2}]},
  {...base,id:'c',workload:'cached',status:'cached',counters:[{rule:'optimization/inline-primitive',detail:'car-cons',count:999}]},
  {...base,id:'d',workload:'old',commit:'other',counters:[{rule:'optimization/inline-primitive',detail:'car-cons',count:1000}]},
  {...base,id:'e',workload:'<script>injected</script>',counters:[{rule:'proof/unknown',detail:'"&',count:1}]},
 ];
 for(const [i,record]of records.entries())fs.writeFileSync(path.join(root,`${i}.json`),JSON.stringify(record));
 fs.writeFileSync(path.join(root,'duplicate.json'),JSON.stringify(records[0]));
 const report=collectCompilerReport(root,{commit:'current',runId:'17',jobs:[{name:'<test>',conclusion:'failure',html_url:'javascript:alert(1)'}]});
 assert.ok(report.records.some(r=>r.workload==='alpha'));
 assert.ok(!report.records.some(r=>r.workload==='old'));
 assert.ok(report.diagnostics.some(d=>d.includes('another commit/run')));
 const html=renderCompilerReport(report);
 assert.ok(html.includes('>5</td>'),'Totals include observed partial events, exclude cached and duplicate records');
 assert.ok(html.includes('2†'));
 assert.ok(html.includes('partial / cached'));
 assert.ok(!html.includes('<script>bad</script>'));
 assert.ok(html.includes('&lt;script&gt;injected&lt;/script&gt;'));
 assert.ok(!html.includes('href="javascript:'));
 assert.ok(html.includes('--paper:#ffffea')&&html.includes('--tag:#eaffff'));
 const empty=renderCompilerReport(collectCompilerReport(root,{commit:'none',jobs:[]}));
 assert.ok(empty.includes('No compiler counters were received.'));
 assert.ok(empty.includes('Job results were not supplied.'));
 console.log('Compiler HTML report: aggregation, duplicate/source filtering, cached and partial coverage, escaping, offline filtering and empty runs pass.');
}finally{fs.rmSync(root,{recursive:true,force:true});}
