{application, grinderl, [
    {description, "Grinderl is a testing tool."},
    {vsn, "0.1"},
    {modules, [
        grd_grinderl_app,
        grd_grinderl_sup,
        grd_random_srv,
        grd_stress_srv,
        grd_worker_srv,
        grd_util
    ]},
    {registered, [
        grd_grinderl_sup,
        grd_random_srv,
        grd_stress_srv
    ]},
    {applications, [
        kernel,
        stdlib,
        sasl
    ]},
    {mod, {grd_grinderl_app, []}},
    {env, [
    ]}
]}.
