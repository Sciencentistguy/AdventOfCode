###
# TEMPORARY
###

import sympy

# Paste the output of `cargo run 21` here
eqn = """

"""

expr = sympy.parsing.sympy_parser.parse_expr(eqn)

ans = sympy.solve(expr)[0]

print(f"Part 2: {ans}")
