// Examples of core functions documented in the upstream Maxima manual.
export const examples = [
  {id: 'integral', title: 'A Gaussian integral', category: 'Calculus',
    note: 'An exact answer to an integral over the whole real line.',
    source: 'Integration.texi.m4 · integrate', manual: 'Integration.html#index-integrate',
    code: 'integrate(x^2 * exp(-x^2), x, minf, inf);', expected: 'sqrt(%pi)/2'},
  {id: 'derivative', title: 'A product and a chain', category: 'Calculus',
    note: 'Differentiate a Gaussian times a sine, then verify the result symbolically.',
    source: 'Differentiation.texi · diff', manual: 'Differentiation.html#index-diff',
    code: 'diff(exp(-x^2)*sin(x),x);\nratsimp(%-exp(-x^2)*(cos(x)-2*x*sin(x)));', expected: '0'},
  {id: 'series', title: 'Expand a logarithm', category: 'Power series',
    note: 'Find the first six terms of log(1+x) around zero.',
    source: 'Series.texi · taylor', manual: 'Sums-Products-and-Series.html#index-taylor',
    code: 'taylor(log(1+x),x,0,6);', expected: 'x^6/6'},
  {id: 'primes', title: 'Take a number apart', category: 'Number theory',
    note: 'Factor a large integer into primes, then multiply the factors to reconstruct it.',
    source: 'Number.texi · ifactors', manual: 'Number-Theory.html#index-ifactors',
    code: 'ifactors(51575319651600);\napply("*",map(lambda([u],u[1]^u[2]),%));', expected: '51575319651600'},
  {id: 'matrix', title: 'Inside a matrix', category: 'Linear algebra',
    note: 'Find exact eigenvalues from the characteristic polynomial.',
    source: 'Matrices.texi.m4 · determinant', manual: 'Matrices-and-Linear-Algebra.html#index-determinant',
    code: 'M1: matrix([11,-1],[1,7])$\np: expand(determinant(M1-z*ident(2)));\nsolve(p,z);', expected: 'sqrt(3)'},
  {id: 'roots', title: 'All the cube roots of one', category: 'Algebra',
    note: 'Solve a polynomial over the complex numbers. Maxima keeps the roots exact.',
    source: 'Equations.texi · solve', manual: 'Equations.html#index-solve',
    code: 'solve(x^3-1);', expected: 'sqrt(3)'},
];

export const config = {name: 'Maxima', storageKey: 'miricl.maxima.sessions.v1', manualBase: 'https://maxima.sourceforge.io/docs/manual/'};
