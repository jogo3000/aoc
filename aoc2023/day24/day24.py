# Idea to use sympy from Ville Puuska
from sympy import cos, nsolve, solve, Symbol

if __name__ == "__main__":
    p_x = Symbol('p_x', real=True)
    v_x = Symbol('v_x', real=True)
    p_y = Symbol('p_y', real=True)
    v_y = Symbol('v_y', real=True)
    p_z = Symbol('p_z', real=True)
    v_z = Symbol('v_z', real=True)
    t_1 = Symbol('t_1', real=True)
    t_2 = Symbol('t_2', real=True)
    t_3 = Symbol('t_3', real=True)
    t_4 = Symbol('t_4', real=True)

    f1 = p_x + t_1 * v_x + 2 * t_1 - 19
    f2 = p_x + t_2 * v_x + t_2 - 18
    f3 = p_x + t_3 * v_x + 2 * t_3 - 20
    f4 = p_x + t_4 * v_x + t_4 - 12

    f5 = p_y + t_1 * v_y - t_1 - 13
    f6 = p_y + t_2 * v_y + t_2 - 19
    f7 = p_y + t_3 * v_y + 2 * t_3 - 25
    f8 = p_y + t_4 * v_y + 2 * t_4 - 31

    f9  = p_z + t_1 * v_z + 2 * t_1 - 30
    f10 = p_z + t_2 * v_z + 2 * t_2 - 22
    f11 = p_z + t_3 * v_z + 4 * t_3 - 34
    f12 = p_z + t_4 * v_z + t_4 - 28

    equations = [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12]

    print(equations)
    print(solve(equations))

    f1 = p_x + t_1 * v_x - 18 * t_1 - 119566840879742
    f2 = p_x + t_2 * v_x + 16 * t_2 - 433973471892198
    f3 = p_x + t_3 * v_x - 197 * t_3 - 44446443386018
    f4 = p_x + t_4 * v_x - 19 * t_4 - 102165762267068

    f5 = p_y + t_1 * v_y + 130 * t_1 - 430566433235378
    f6 = p_y + t_2 * v_y + 170 * t_2 - 260061119249300
    f7 = p_y + t_3 * v_y - 16 * t_3 - 281342848485672
    f8 = p_y + t_4 * v_y - 15 * t_4 - 293235409083300

    f9  = p_z + t_1 * v_z - 74 * t_1 - 268387686114969
    f10 = p_z + t_2 * v_z + 118 * t_2 - 263051300077633
    f11 = p_z + t_3 * v_z - 200 * t_3 - 166638492241385
    f12 = p_z + t_4 * v_z - 9 * t_4 - 334966976680379

    equations = [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12]

    print(equations)
    print(solve(equations))
