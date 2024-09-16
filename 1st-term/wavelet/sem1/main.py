import sympy as sp


def get_derivative(function: sp.Expr, symbol: sp.Symbol) -> sp.Expr:
    return sp.diff(function, symbol, 1)


def get_integral(
        function: sp.Expr, symbol: sp.Symbol,
        lims: tuple[int, int] | list[int] | None = None,
) -> sp.Expr:
    variable = symbol
    if lims:
        variable = (symbol, *lims)

    return sp.integrate(function, variable)


def print_first_task():
    print('┏━━━━━━━━━┫TASK 1┣━━━━━━━━━━┓')

    def function(_t):
        return pow(_t, 2) + 2 * sp.sin(sp.pi * _t) + 4 * sp.cos(3 * sp.pi * _t)

    t = sp.Symbol('t')

    sp.plot(function(t), xlim=[-4, 4])

    print(f"f(t)={function(t)}")
    derivative: sp.Expr = get_derivative(function(t), t)
    print(f"f'(t)={derivative}")
    print(f"f'(2)={derivative.subs({str(t): 2})}")

    integral: sp.Expr = get_integral(function(t), t)
    print(f"∫f(t)={integral}")
    integral: sp.Expr = get_integral(function(t), t, lims=[-4, 4])
    print(f"∫f(t) from -4 to 4={integral}={integral.evalf()}")
    print('┗━━━━━━━━━┫TASK 1┣━━━━━━━━━━┛')


def print_second_task():
    print('┏━━━━━━━━━┫TASK 2┣━━━━━━━━━━┓')
    t = sp.Symbol('t')

    def function(_t):
        return pow(_t, 2) + 2 * sp.sin(sp.pi * _t) + 4 * sp.cos(3 * sp.pi * _t)

    s: sp.Expr = sp.series(function(t), t, 0, 3)
    print(f's(t)={s}')

    sp.plot([function(t), s.removeO()], xlim=[-0.5, 0.5])
    sp.plot([function(t), s.removeO()], xlim=[-1, 1])
    sp.plot([function(t), s.removeO()], xlim=[-4, 4])

    s: sp.Expr = sp.series(function(t), t, 0, 5)
    print(f's(t)={s}')
    sp.plot([function(t), s.removeO()], xlim=[-1, 1])
    print('┗━━━━━━━━━┫TASK 2┣━━━━━━━━━━┛')


def print_third_task():
    print('┏━━━━━━━━━┫TASK 3┣━━━━━━━━━━┓')
    t = sp.Symbol('t')

    def function(_t):
        return pow(_t, 2) + 2 * sp.sin(sp.pi * _t) + 4 * sp.cos(3 * sp.pi * _t)

    r = sp.fourier_series(function(t), (t, -4, 4))
    print(f'r(t)={r.truncate(3)}')
    sp.plot([function(t), r], xlim=[-4, 4])
    print(f'r(t)={r.truncate(5)}')
    sp.plot([function(t), r], xlim=[-4, 4])

    print('┗━━━━━━━━━┫TASK 3┣━━━━━━━━━━┛')


def print_fourth_task():
    print('┏━━━━━━━━━┫TASK 4┣━━━━━━━━━━┓')
    t, n = sp.symbols('t n', real=True)

    # Задаем функцию f(t)
    f_t = t ** 2 + 2 * sp.sin(sp.pi * t) + 4 * sp.cos(3 * sp.pi * t)

    # Определяем период
    T = 2

    # Выражения для коэффициентов Фурье
    a_0 = (1 / T) * sp.integrate(f_t, (t, 0, T))

    a_n = (2 / T) * sp.integrate(
        f_t * sp.cos(2 * sp.pi * n * t / T), (t, 0, T)
        )
    b_n = (2 / T) * sp.integrate(
        f_t * sp.sin(2 * sp.pi * n * t / T), (t, 0, T)
        )

    # Упрощение выражений
    a_0_simplified = sp.simplify(a_0)
    a_n_simplified = sp.simplify(a_n)
    b_n_simplified = sp.simplify(b_n)

    print(a_0_simplified, a_n_simplified, b_n_simplified)
    print('┗━━━━━━━━━┫TASK 2┣━━━━━━━━━━┛')


if __name__ == '__main__':
    print_first_task()
    print()
    print_second_task()
    print()
    print_third_task()
    print()
    print_fourth_task()
