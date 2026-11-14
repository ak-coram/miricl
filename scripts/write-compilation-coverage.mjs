// Preserve the distinction between a reused build and zero optimization use.
import {writeCompilationStatistics} from './compilation-statistics.mjs';
const [workload, cached] = process.argv.slice(2);
if (!workload) throw new Error('Expected WORKLOAD [CACHE-HIT]');
if (cached === 'true') await writeCompilationStatistics({workload, phase:'native build cache',
  status:'cached', counters:[], reason:'Native compilation was reused; no current-run counters.'});
