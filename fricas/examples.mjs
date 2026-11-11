// Commands transcribed from the pinned upstream HyperDoc examples.
export const config = {name: 'FriCAS', storageKey: 'miricl.fricas.sessions.v1',
  manualBase: 'https://fricas.github.io/'};
export const examples = [
  {id: 'derivative', category: 'CALCULUS', title: 'A product, differentiated',
    code: 'differentiate(sin(x) * exp(x^2),x)', expected: 'cos(x)', checks: ['2 x %e', 'sin(x) + cos(x)%e'],
    note: 'Combine the product and chain rules symbolically.', manual: '', source: 'Computing Derivatives'},
  {id: 'parameter-integral', category: 'SYMBOLIC INTEGRATION', title: 'One integral, several answers',
    code: 'integrate(1/(x^2 + a),x)', expected: 'atan', checks: ['log(', 'Type: Union(List(Expression(Integer)),...)'],
    note: 'FriCAS returns alternatives depending on the sign of the real parameter a.', manual: '', source: 'Integral with a Real Parameter'},
  {id: 'series', category: 'LAZY POWER SERIES', title: 'An infinite sine, a term at a time',
    code: 'series(sin(a*x),x = 0)', expected: 'UnivariatePuiseuxSeries', checks: ['39916800', 'a x -'],
    note: 'The displayed terms belong to a lazy series. More coefficients are computed when requested.', manual: '', source: 'Converting Expressions to Series'},
  {id: 'series-arithmetic', category: 'EXACT ARITHMETIC', title: 'Square a geometric series',
    code: 'f := series(1/(1-x),x = 0)\nf ^ 2', expected: 'UnivariatePuiseuxSeries', checks: ['1 + 2 x + 3 x'],
    note: 'Arithmetic on series produces the sequence 1, 2, 3, … as coefficients.', manual: '', source: 'Manipulating Power Series'},
  {id: 'limit', category: 'LIMITS', title: 'Resolve a zero over zero',
    code: 'limit((x^2 - 3*x + 2)/(x^2 - 1),x = 1)', expected: 'Type: Union', pattern: '\\d+\n\\s+\\(\\d+\\)\\s+- -\n\\s+2',
    note: 'Both numerator and denominator vanish. The two-sided limit is −1/2.', manual: '', source: 'Computing Limits'},
  {id: 'matrix', category: 'LINEAR ALGEBRA', title: 'Build a matrix from blocks',
    code: 'horizConcat(matrix([[1,2,3],[6,7,8]]),matrix([[11,12,13],[55,77,88]]))', expected: 'Matrix(Integer)', checks: ['+1  2  3  11  12  13+', '+6  7  8  55  77  88+'],
    note: 'Join two matrices horizontally while preserving their exact integer type.', manual: '', source: 'Constructing new Matrices'},
];
