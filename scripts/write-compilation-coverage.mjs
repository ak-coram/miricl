// Preserve the distinction between a reused build and zero optimization use.
import {writeCompilationStatistics} from './compilation-statistics.mjs';
const [workload, cached, phase = 'native build cache'] = process.argv.slice(2);
if (!workload) throw new Error('Expected WORKLOAD [CACHE-HIT]');
if (cached === 'true') await writeCompilationStatistics({workload, phase, compilationMode: 'native',
  workloadScope: workload === 'compiler-bootstrap' ? 'compiler' : 'application',
  status:'cached', counters:[], reason:'Compilation artifacts were reused; no current-run counters.'});
